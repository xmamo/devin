{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Devin.Evaluators (
  evaluateDevin,
  evaluateDeclaration,
  evaluateStatement,
  evaluateExpression
) where

import Data.Bits
import Data.Foldable
import Data.Traversable
import Numeric

import Data.Vector ((!), (!?))
import Data.Vector qualified as Vector

import Data.Foldable.Extra

import Devin.Error
import Devin.Evaluator
import Devin.Syntax
import Devin.Type (Type)
import Devin.Type qualified as Type


evaluateDevin :: Devin -> Evaluator ()
evaluateDevin Devin {declarations} = do
  for_ declarations $ \declaration -> case declaration of
    VariableDeclaration {} -> pure ()
    FunctionDeclaration {} -> evaluateDeclaration declaration

  Just (UserDefined FunctionDeclaration {parameters = [], body}, depth) <- lookupFunction "main"

  for_ declarations $ \declaration -> case declaration of
    VariableDeclaration {} -> evaluateDeclaration declaration
    FunctionDeclaration {} -> pure ()

  withNewFrame (depth + 1) (evaluateStatement body)
  pure ()


evaluateDeclaration :: Declaration -> Evaluator ()
evaluateDeclaration = onDeclaration $ \declaration -> case declaration of
  VariableDeclaration {variableId = SymbolId {name}, value} -> do
    r <- evaluateExpression value
    r' <- cloneReference r
    defineVariable name r'

  FunctionDeclaration {functionId = SymbolId {name}} -> do
    yield (BeforeDeclaration declaration)
    defineFunction name (UserDefined declaration)
    yield (AfterDeclaration declaration)


evaluateStatement :: Statement -> Evaluator (Maybe Reference)
evaluateStatement = onStatement $ \statement -> case statement of
  DeclarationStatement {declaration} -> do
    evaluateDeclaration declaration
    pure Nothing

  ExpressionStatement {value} -> do
    evaluateExpression value
    pure Nothing

  IfStatement {predicate, trueBranch} -> do
    r <- evaluateExpression predicate
    v <- readReference r

    case v of
      Bool True -> withNewFrame 1 (evaluateStatement trueBranch)
      Bool False -> pure Nothing

      _ -> do
        t <- getType v
        raise (InvalidType predicate Type.Bool t)

  IfElseStatement {predicate, trueBranch, falseBranch} -> do
    r <- evaluateExpression predicate
    v <- readReference r

    case v of
      Bool True -> withNewFrame 1 (evaluateStatement trueBranch)
      Bool False -> withNewFrame 1 (evaluateStatement falseBranch)

      _ -> do
        t <- getType v
        raise (InvalidType predicate Type.Bool t)

  WhileStatement {predicate, body} -> do
    r <- evaluateExpression predicate
    v <- readReference r

    case v of
      Bool False -> pure Nothing

      Bool True ->
        withNewFrame 1 (evaluateStatement body) >>= \case
          Just r -> pure (Just r)
          Nothing -> evaluateStatement statement

      _ -> do
        t <- getType v
        raise (InvalidType predicate Type.Bool t)

  DoWhileStatement {body, predicate} ->
    withNewFrame 1 (evaluateStatement body) >>= \case
      Just r -> pure (Just r)

      Nothing -> do
        r <- evaluateExpression predicate
        v <- readReference r

        case v of
          Bool False -> pure Nothing
          Bool True -> evaluateStatement statement

          _ -> do
            t <- getType v
            raise (InvalidType predicate Type.Bool t)

  ReturnStatement {result = Just result} -> do
    r <- evaluateExpression result
    pure (Just r)

  ReturnStatement {result = Nothing} -> pure Nothing

  AssertStatement {predicate} -> do
    r <- evaluateExpression predicate
    v <- readReference r

    case v of
      Bool False -> raise (AssertionFailed statement)
      Bool True -> pure Nothing

      _ -> do
        t <- getType v
        raise (InvalidType predicate Type.Bool t)

  BlockStatement {statements} -> withNewFrame 1 $ do
    for_ statements $ \case
      DeclarationStatement {declaration} | FunctionDeclaration {} <- declaration ->
        evaluateDeclaration declaration

      _ -> pure ()

    let evaluate DeclarationStatement {declaration = FunctionDeclaration {}} = pure Nothing
        evaluate statement = evaluateStatement statement

    firstJustM evaluate statements


evaluateExpression :: Expression -> Evaluator Reference
evaluateExpression = onExpression $ \expression -> case expression of
  IntegerExpression {integer} | Just x <- toIntegralSized integer -> newReference (Int x)
  IntegerExpression {} -> raise (IntegerOverflow expression)

  RationalExpression {rational} -> newReference (Float (fromRat rational))

  VariableExpression {variableName, interval} ->
    lookupVariable variableName >>= \case
      Just (r, _) -> pure r
      Nothing -> raise (UnknownVariable variableName interval)

  ArrayExpression {elements} -> do
    rs <- Vector.unfoldrM f elements
    newReference (Array rs)

    where
      f [] = pure Nothing

      f (element : elements) = do
        r <- evaluateExpression element
        r' <- cloneReference r
        pure (Just (r', elements))

  AccessExpression {array, index} -> do
    arrayR <- evaluateExpression array
    arrayV <- readReference arrayR

    indexR <- evaluateExpression index
    indexV <- readReference indexR

    case (arrayV, indexV) of
      (Array rs, Int y) | Just y' <- toIntegralSized y, Just r <- rs !? y' -> pure r

      (Array _, Int y) -> raise (IndexOutOfBounds index y)

      (Array _, _) -> do
        indexT <- getType indexV
        raise (InvalidType index Type.Int indexT)

      (_, _) -> do
        arrayT <- getType arrayV
        raise (InvalidType array (Type.Array Type.Unknown) arrayT)

  CallExpression {functionId = SymbolId {name, interval}, arguments} -> do
    argumentRs <- for arguments evaluateExpression

    lookupFunction name >>= \case
      Just (UserDefined FunctionDeclaration {parameters, body}, depth) ->
        withNewFrame (depth + 1) (go 0 parameters argumentRs)
        where
          go _ [] [] = evaluateStatement body >>= \case
            Just r -> cloneReference r
            Nothing -> newReference Unit

          -- Pass by value
          go n ((Nothing, SymbolId {name}, _) : parameters) (argumentR : argumentRs) = do
            argumentR' <- cloneReference argumentR
            defineVariable name argumentR'
            go (n + 1) parameters argumentRs

          -- Pass by reference
          go n ((Just _, SymbolId {name}, _) : parameters) (argumentR : argumentRs) = do
            defineVariable name argumentR
            go (n + 1) parameters argumentRs

          go n parameters argumentRs =
            raise (InvalidArgumentCount expression (n + length parameters) (n + length argumentRs))

      Just (BuiltinToInt, depth) -> withNewFrame (depth + 1) $ case argumentRs of
        [] -> raise (InvalidArgumentCount expression 1 0)

        argumentR : argumentRs -> do
          argumentV <- readReference argumentR

          case (argumentV, argumentRs) of
            (Float x, []) -> newReference (Int (round x))

            (_, []) -> do
              argumentT <- getType argumentV
              raise (InvalidType (head arguments) Type.Float argumentT)

            (_, _) -> raise (InvalidArgumentCount expression 1 (1 + length argumentRs))

      Just (BuiltinToFloat, depth) -> withNewFrame (depth + 1) $ case argumentRs of
        [] -> raise (InvalidArgumentCount expression 1 0)

        argumentR : argumentRs -> do
          argumentV <- readReference argumentR

          case (argumentV, argumentRs) of
            (Int x, []) -> newReference (Float (fromIntegral x))

            (_, []) -> do
              argumentT <- getType argumentV
              raise (InvalidType (head arguments) Type.Int argumentT)

            (_, _) -> raise (InvalidArgumentCount expression 1 (1 + length argumentRs))

      _ -> raise (UnknownFunction name interval)

  UnaryExpression {unary, operand} | PlusOperator {} <- unary -> do
    operandR <- evaluateExpression operand
    operandV <- readReference operandR

    case operandV of
      Int x -> newReference (Int x)
      Float x -> newReference (Float x)

      _ -> do
        operandT <- getType operandV
        raise (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | MinusOperator {} <- unary -> do
    operandR <- evaluateExpression operand
    operandV <- readReference operandR

    case operandV of
      Int x | Just y <- safeUnary negate x -> newReference (Int y)
      Int _ -> raise (IntegerOverflow expression)

      Float x -> newReference (Float (negate x))

      _ -> do
        operandT <- getType operandV
        raise (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | NotOperator {} <- unary -> do
    operandR <- evaluateExpression operand
    operandV <- readReference operandR

    case operandV of
      Bool x -> newReference (Bool (not x))

      _ -> do
        operandT <- getType operandV
        raise (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | LenOperator {} <- unary -> do
    operandR <- evaluateExpression operand
    operandV <- readReference operandR

    case operandV of
      Array rs -> newReference (Int (fromIntegral (length rs)))

      _ -> do
        operandT <- getType operandV
        raise (InvalidUnary unary operandT)

  BinaryExpression {left, binary, right} | AddOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (+) x y -> newReference (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newReference (Float (x + y))

      (Array rs1, Array rs2) -> do
        let n1 = Vector.length rs1
        let n2 = Vector.length rs2

        case safeBinary (+) n1 n2 of
          Nothing -> raise (IntegerOverflow expression)

          Just n3 -> do
            let f i = cloneReference (if i < n1 then rs1 ! i else rs2 ! (i - n1))
            rs3 <- Vector.generateM n3 f
            newReference (Array rs3)

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | SubtractOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (-) x y -> newReference (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newReference (Float (x - y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | MultiplyOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (*) x y -> newReference (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newReference (Float (x * y))

      (Int x, Array _) | x <= 0 -> newReference (Array Vector.empty)
      (Array _, Int y) | y <= 0 -> newReference (Array Vector.empty)

      (Int x, Array rs) -> do
        let n = Vector.length rs

        case safeBinary (+) x n of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            rs' <- Vector.generateM n' (\i -> cloneReference (rs ! (i `mod` n)))
            newReference (Array rs')

      (Array rs, Int y) -> do
        let n = Vector.length rs

        case safeBinary (+) n y of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            rs' <- Vector.generateM n' (\i -> cloneReference (rs ! (i `mod` n)))
            newReference (Array rs')

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | DivideOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary div x y -> newReference (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newReference (Float (x / y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | ModuloOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary mod x y -> newReference (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary = EqualOperator {}, right} -> do
    leftR <- evaluateExpression left
    rightR <- evaluateExpression right
    x <- compareReferences leftR rightR
    newReference (Bool x)

  BinaryExpression {left, binary = NotEqualOperator {}, right} -> do
    leftR <- evaluateExpression left
    rightR <- evaluateExpression right
    x <- compareReferences leftR rightR
    newReference (Bool (not x))

  BinaryExpression {left, binary, right} | LessOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) -> newReference (Bool (x < y))
      (Float x, Float y) -> newReference (Bool (x < y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | LessOrEqualOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) -> newReference (Bool (x <= y))
      (Float x, Float y) -> newReference (Bool (x <= y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | GreaterOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) -> newReference (Bool (x > y))
      (Float x, Float y) -> newReference (Bool (x > y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | GreaterOrEqualOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) -> newReference (Bool (x >= y))
      (Float x, Float y) -> newReference (Bool (x >= y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | AndOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    case leftV of
      Bool False -> newReference (Bool False)

      _ -> do
        rightR <- evaluateExpression right
        rightV <- readReference rightR

        case (leftV, rightV) of
          (Bool x, Bool y) -> newReference (Bool (x && y))

          (_, _) -> do
            leftT <- getType leftV
            rightT <- getType rightV
            raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | OrOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    case leftV of
      Bool True -> newReference (Bool True)

      _ -> do
        rightR <- evaluateExpression right
        rightV <- readReference rightR

        case (leftV, rightV) of
          (Bool x, Bool y) -> newReference (Bool (x || y))

          (_, _) -> do
            leftT <- getType leftV
            rightT <- getType rightV
            raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | XorOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Bool x, Bool y) -> newReference (Bool (x /= y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary = PlainAssignOperator {}, right} -> do
    leftR <- evaluateExpression left

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    writeReference' leftR rightV

  BinaryExpression {left, binary, right} | AddAssignOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (+) x y -> writeReference' leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeReference' leftR (Float (x + y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | SubtractAssignOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (-) x y -> writeReference' leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeReference' leftR (Float (x - y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | MultiplyAssignOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int x, Int y) | Just z <- safeBinary (*) x y -> writeReference' leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeReference' leftR (Float (x * y))

      (Array rs, Int y) -> do
        let n = Vector.length rs

        case safeBinary (*) n y of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            rs' <- Vector.generateM n' (\i -> cloneReference (rs ! (i `mod` n)))
            writeReference' leftR (Array rs')

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | DivideAssignOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary div x y -> writeReference' leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeReference' leftR (Float (x / y))

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | ModuloAssignOperator {} <- binary -> do
    leftR <- evaluateExpression left
    leftV <- readReference leftR

    rightR <- evaluateExpression right
    rightV <- readReference rightR

    case (leftV, rightV) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary mod x y -> writeReference' leftR (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (_, _) -> do
        leftT <- getType leftV
        rightT <- getType rightV
        raise (InvalidBinary binary leftT rightT)

  ParenthesizedExpression {inner} -> evaluateExpression inner

  where
    safeUnary op x = toIntegralSized (toInteger (op x))
    safeBinary op x y = toIntegralSized (toInteger x `op` toInteger y)


getType :: Value -> Evaluator Type
getType = \case
  Unit -> pure Type.Unit
  Bool _ -> pure Type.Bool
  Int _ -> pure Type.Int
  Float _ -> pure Type.Float

  Array rs | Vector.null rs -> pure (Type.Array Type.Unknown)

  Array rs -> do
    r <- readReference (Vector.head rs)
    t <- getType r

    let f Type.Unknown = False; f t' = t' == t
    ok <- allM (\r -> f <$> (getType =<< readReference r)) rs
    pure (Type.Array (if ok then t else Type.Unknown))


writeReference' :: Reference -> Value -> Evaluator Reference
writeReference' r v = do
  writeReference r v
  pure r
