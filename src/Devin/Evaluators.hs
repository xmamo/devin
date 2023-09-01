{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Evaluators (
  evalDevin,
  evalDefinition,
  evalStatement,
  evalExpression
) where

import Data.Bits
import Data.Foldable
import Data.Traversable
import Numeric

import Data.Vector ((!), (!?))
import qualified Data.Vector as Vector

import Control.Monad.Extra hiding (allM)
import Data.Foldable.Extra (allM)

import Devin.Error
import Devin.Evaluator
import Devin.Syntax
import Devin.Type (Type, (<:))
import qualified Devin.Type as Type


evalDevin :: Devin -> Evaluator ()
evalDevin Devin {definitions} = do
  for_ definitions evalDefinition1
  for_ definitions evalDefinition2
  Just (UserDefined FunDefinition {params = [], body}, depth) <- lookupFun "main"
  withNewFrame (Just "main") (depth + 1) (evalStatement body)
  pure ()


evalDefinition :: Definition -> Evaluator ()
evalDefinition definition = do
  evalDefinition1 definition
  evalDefinition2 definition


evalDefinition1 :: Definition -> Evaluator ()
evalDefinition1 definition = case definition of
  VarDefinition {} -> pure ()

  FunDefinition {funId = SymbolId {name}} ->
    defineFun name (UserDefined definition)


evalDefinition2 :: Definition -> Evaluator ()
evalDefinition2 = \case
  VarDefinition {varId = SymbolId {name}, value} -> do
    r <- evalExpression value
    r' <- cloneRef r
    defineVar name r'

  FunDefinition {} -> pure ()


evalStatement :: Statement -> Evaluator (Maybe Reference)
evalStatement statement = case statement of
  DefinitionStatement {definition} -> do
    evalDefinition definition
    pure Nothing

  ExpressionStatement {value} -> do
    evalExpression value
    pure Nothing

  IfStatement {predicate, trueBranch} -> do
    r <- evalExpression predicate
    v <- readRef r

    case v of
      Bool True -> withNewFrame Nothing 1 (evalStatement trueBranch)
      Bool False -> pure Nothing

      _ -> do
        t <- getType v
        raise (InvalidType predicate Type.Bool t)

  IfElseStatement {predicate, trueBranch, falseBranch} -> do
    r <- evalExpression predicate
    v <- readRef r

    case v of
      Bool True -> withNewFrame Nothing 1 (evalStatement trueBranch)
      Bool False -> withNewFrame Nothing 1 (evalStatement falseBranch)

      _ -> do
        t <- getType v
        raise (InvalidType predicate Type.Bool t)

  WhileStatement {predicate, body} -> do
    r <- evalExpression predicate
    v <- readRef r

    case v of
      Bool False -> pure Nothing

      Bool True -> withNewFrame Nothing 1 (evalStatement body) >>= \case
        Just r -> pure (Just r)
        Nothing -> evalStatement statement

      _ -> do
        t <- getType v
        raise (InvalidType predicate Type.Bool t)

  DoWhileStatement {body, predicate} ->
    withNewFrame Nothing 1 (evalStatement body) >>= \case
      Just r -> pure (Just r)

      Nothing -> do
        r <- evalExpression predicate
        v <- readRef r

        case v of
          Bool False -> pure Nothing
          Bool True -> evalStatement statement

          _ -> do
            t <- getType v
            raise (InvalidType predicate Type.Bool t)

  ReturnStatement {result = Just result} -> do
    r <- evalExpression result
    pure (Just r)

  ReturnStatement {result = Nothing} -> do
    r <- newRef Unit
    pure (Just r)

  AssertStatement {predicate} -> do
    r <- evalExpression predicate
    v <- readRef r

    case v of
      Bool False -> raise (AssertionFailed statement)
      Bool True -> pure Nothing

      _ -> do
        t <- getType v
        raise (InvalidType predicate Type.Bool t)

  BreakpointStatement {} -> do
    breakpoint statement
    pure Nothing

  BlockStatement {statements} -> withNewFrame Nothing 1 $ do
    for_ statements $ \case
      DefinitionStatement {definition} -> evalDefinition1 definition
      _ -> pure ()

    flip firstJustM statements $ \case
      DefinitionStatement {definition} -> do
        evalDefinition2 definition
        pure Nothing

      statement -> evalStatement statement


evalExpression :: Expression -> Evaluator Reference
evalExpression expression = case expression of
  IntegerExpression {integer} -> case toIntegralSized integer of
    Just x -> newRef (Int x)
    Nothing -> raise (IntegerOverflow expression)

  RationalExpression {rational} -> newRef (Float (fromRat rational))

  VarExpression {varName, interval} -> lookupVar varName >>= \case
    Just (r, _) -> pure r
    Nothing -> raise (UnknownVar varName interval)

  ArrayExpression {elems} -> do
    rs <- flip Vector.unfoldrM elems $ \case
      [] -> pure Nothing

      (elem : elems) -> do
        r <- evalExpression elem
        r' <- cloneRef r
        pure (Just (r', elems))

    newRef (Array rs)

  AccessExpression {array, index} -> do
    arrayR <- evalExpression array
    arrayV <- readRef arrayR

    indexR <- evalExpression index
    indexV <- readRef indexR

    case (arrayV, indexV) of
      (Array rs, Int x) -> case toIntegralSized x of
        Just n | Just r <- rs !? n -> pure r
        _ -> raise (IndexOutOfBounds index x)

      (Array _, _) -> do
        indexT <- getType indexV
        raise (InvalidType index Type.Int indexT)

      (_, _) -> do
        arrayT <- getType arrayV
        raise (InvalidType array (Type.Array Type.Unknown) arrayT)

  CallExpression {funId = SymbolId {name, interval}, args} -> do
    argRs <- for args evalExpression

    lookupFun name >>= \case
      Just (UserDefined FunDefinition {params, body}, depth) ->
        withNewFrame (Just name) (depth + 1) (go 0 params argRs)

        where
          -- Pass argument by value:
          go n ((Nothing, SymbolId {name}, _) : params) (argR : argRs) = do
            argR' <- cloneRef argR
            defineVar name argR'
            go (n + 1) params argRs

          -- Pass argument by reference:
          go n ((Just _, SymbolId {name}, _) : params) (argR : argRs) = do
            defineVar name argR
            go (n + 1) params argRs

          -- If argument count is correct:
          go _ [] [] = evalStatement body >>= \case
            Just r -> cloneRef r
            Nothing -> newRef Unit

          -- If argument count is incorrect:
          go n params argRs = do
            let expected = n + length params
            let actual = n + length argRs
            raise (InvalidArgCount expression expected actual)

      Just (BuiltinToInt, depth) ->
        withNewFrame (Just name) (depth + 1) $ case argRs of
          [argR] -> do
            argV <- readRef argR

            case argV of
              Float x -> newRef (Int (round x))

              _ -> do
                argT <- getType argV
                raise (InvalidType (head args) Type.Float argT)

          _ -> raise (InvalidArgCount expression 1 (length argRs))

      Just (BuiltinToFloat, depth) ->
        withNewFrame (Just name) (depth + 1) $ case argRs of
          [argR] -> do
            argV <- readRef argR

            case argV of
              Int x -> newRef (Float (fromIntegral x))

              _ -> do
                argT <- getType argV
                raise (InvalidType (head args) Type.Int argT)

          _ -> raise (InvalidArgCount expression 1 (length argRs))

      _ -> raise (UnknownFun name interval)

  UnaryExpression {unary, operand} | PlusOperator {} <- unary -> do
    operandR <- evalExpression operand
    operandV <- readRef operandR

    case operandV of
      Int x -> newRef (Int x)
      Float x -> newRef (Float x)

      _ -> do
        operandT <- getType operandV
        raise (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | MinusOperator {} <- unary -> do
    operandR <- evalExpression operand
    operandV <- readRef operandR

    case operandV of
      Int x | Just y <- safeUnary negate x -> newRef (Int y)
      Int _ -> raise (IntegerOverflow expression)

      Float x -> newRef (Float (negate x))

      _ -> do
        operandT <- getType operandV
        raise (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | NotOperator {} <- unary -> do
    operandR <- evalExpression operand
    operandV <- readRef operandR

    case operandV of
      Bool x -> newRef (Bool (not x))

      _ -> do
        operandT <- getType operandV
        raise (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | LenOperator {} <- unary -> do
    operandR <- evalExpression operand
    operandV <- readRef operandR

    case operandV of
      Array rs -> newRef (Int (fromIntegral (length rs)))

      _ -> do
        operandT <- getType operandV
        raise (InvalidUnary unary operandT)

  BinaryExpression {left, binary, right} | AddOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (+) x y -> newRef (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newRef (Float (x + y))

      (Array rs1, Array rs2) -> do
        let n1 = Vector.length rs1
        let n2 = Vector.length rs2

        case safeBinary (+) n1 n2 of
          Nothing -> raise (IntegerOverflow expression)

          Just n3 -> do
            let f i = cloneRef (if i < n1 then rs1 ! i else rs2 ! (i - n1))
            rs3 <- Vector.generateM n3 f
            newRef (Array rs3)

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | SubtractOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (-) x y -> newRef (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newRef (Float (x - y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | MultiplyOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (*) x y -> newRef (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newRef (Float (x * y))

      (Int x, Array _) | x <= 0 -> newRef (Array Vector.empty)
      (Array _, Int y) | y <= 0 -> newRef (Array Vector.empty)

      (Int x, Array rs) -> do
        let n = Vector.length rs

        case safeBinary (*) x n of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            rs' <- Vector.generateM n' (\i -> cloneRef (rs ! (i `mod` n)))
            newRef (Array rs')

      (Array rs, Int y) -> do
        let n = Vector.length rs

        case safeBinary (*) n y of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            rs' <- Vector.generateM n' (\i -> cloneRef (rs ! (i `mod` n)))
            newRef (Array rs')

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | DivideOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary div x y -> newRef (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newRef (Float (x / y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | ModuloOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary mod x y -> newRef (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary = EqualOperator {}, right} -> do
    leftR <- evalExpression left
    rightR <- evalExpression right
    x <- compareRefs leftR rightR
    newRef (Bool x)

  BinaryExpression {left, binary = NotEqualOperator {}, right} -> do
    leftR <- evalExpression left
    rightR <- evalExpression right
    x <- compareRefs leftR rightR
    newRef (Bool (not x))

  BinaryExpression {left, binary, right} | LessOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) -> newRef (Bool (x < y))
      (Float x, Float y) -> newRef (Bool (x < y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | LessOrEqualOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) -> newRef (Bool (x <= y))
      (Float x, Float y) -> newRef (Bool (x <= y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | GreaterOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) -> newRef (Bool (x > y))
      (Float x, Float y) -> newRef (Bool (x > y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | GreaterOrEqualOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) -> newRef (Bool (x >= y))
      (Float x, Float y) -> newRef (Bool (x >= y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | AndOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    case leftV of
      Bool False -> newRef (Bool False)

      _ -> do
        rightR <- evalExpression right
        rightV <- readRef rightR

        case (leftV, rightV) of
          (Bool x, Bool y) -> newRef (Bool (x && y))

          (_, _) -> do
            leftT <- getType leftV
            rightT <- getType rightV
            raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | OrOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    case leftV of
      Bool True -> newRef (Bool True)

      _ -> do
        rightR <- evalExpression right
        rightV <- readRef rightR

        case (leftV, rightV) of
          (Bool x, Bool y) -> newRef (Bool (x || y))

          (_, _) -> do
            leftT <- getType leftV
            rightT <- getType rightV
            raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | XorOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Bool x, Bool y) -> newRef (Bool (x /= y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary = PlainAssignOperator {}, right} -> do
    leftR <- evalExpression left

    rightR <- evalExpression right
    rightV <- readRef rightR
    rightV' <- cloneVal rightV

    writeRef leftR rightV'

  BinaryExpression {left, binary, right} | AddAssignOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (+) x y -> writeRef leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeRef leftR (Float (x + y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | SubtractAssignOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (-) x y -> writeRef leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeRef leftR (Float (x - y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | MultiplyAssignOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (*) x y -> writeRef leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeRef leftR (Float (x * y))

      (Array rs, Int y) -> do
        let n = Vector.length rs

        case safeBinary (*) n y of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            rs' <- Vector.generateM n' (\i -> cloneRef (rs ! (i `mod` n)))
            writeRef leftR (Array rs')

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | DivideAssignOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary div x y -> writeRef leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeRef leftR (Float (x / y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | ModuloAssignOperator {} <- binary -> do
    leftR <- evalExpression left
    leftV <- readRef leftR

    rightR <- evalExpression right
    rightV <- readRef rightR

    case (leftV, rightV) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary mod x y -> writeRef leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  ParenthesizedExpression {inner} -> evalExpression inner

  where
    safeUnary op x = toIntegralSized (op (toInteger x))
    safeBinary op x y = toIntegralSized (toInteger x `op` toInteger y)


getType :: Value -> Evaluator Type
getType = \case
  Unit -> pure Type.Unit
  Bool _ -> pure Type.Bool
  Int _ -> pure Type.Int
  Float _ -> pure Type.Float

  Array rs | Vector.null rs -> pure (Type.Array Type.Unknown)

  Array rs -> do
    r <- readRef (Vector.head rs)
    t <- getType r

    let f Type.Unknown = False
        f t' = t' <: t

    ok <- allM (\r -> f <$> (getType =<< readRef r)) rs
    pure (Type.Array (if ok then t else Type.Unknown))
