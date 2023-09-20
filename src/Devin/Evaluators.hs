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

import qualified Data.Vector as Vector
import Data.Vector ((!), (!?))

import Control.Monad.Extra

import Devin.Error
import Devin.Evaluator
import qualified Devin.Type as Type
import Devin.Syntax


evalDevin :: Devin -> Evaluator ()
evalDevin Devin {definitions} = void $ do
  for_ definitions evalDefinition1
  for_ definitions evalDefinition2
  Just (UserDefined FunDefinition {params = [], body}, depth) <- lookupFun "main"
  withNewFrame (Just "main") (depth + 1) (evalStatement body)


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
    cell <- evalExpression value
    cell' <- cloneCell cell
    defineVar name cell'

  FunDefinition {} -> pure ()


evalStatement :: Statement -> Evaluator (Maybe Cell)
evalStatement statement = do
  yield statement

  case statement of
    DefinitionStatement {definition} -> do
      evalDefinition definition
      pure Nothing

    ExpressionStatement {value} -> do
      evalExpression value
      pure Nothing

    IfStatement {predicate, trueBranch} -> do
      cell <- evalExpression predicate
      val <- readCell cell

      case val of
        Bool True -> withNewFrame Nothing 1 (evalStatement trueBranch)
        Bool False -> pure Nothing

        _ -> do
          t <- getType val
          raise (InvalidType predicate Type.Bool t)

    IfElseStatement {predicate, trueBranch, falseBranch} -> do
      val <- evalExpression predicate
      cell <- readCell val

      case cell of
        Bool True -> withNewFrame Nothing 1 (evalStatement trueBranch)
        Bool False -> withNewFrame Nothing 1 (evalStatement falseBranch)

        _ -> do
          t <- getType cell
          raise (InvalidType predicate Type.Bool t)

    WhileStatement {predicate, body} -> go
      where
        go = do
          cell <- evalExpression predicate
          val <- readCell cell

          case val of
            Bool False -> pure Nothing

            Bool True -> withNewFrame Nothing 1 (evalStatement body) >>= \case
              Just cell -> pure (Just cell)
              Nothing -> go

            _ -> do
              t <- getType val
              raise (InvalidType predicate Type.Bool t)

    DoWhileStatement {body, predicate} -> go
      where
        go =
          withNewFrame Nothing 1 (evalStatement body) >>= \case
            Just cell -> pure (Just cell)

            Nothing -> do
              cell <- evalExpression predicate
              val <- readCell cell

              case val of
                Bool False -> pure Nothing
                Bool True -> go

                _ -> do
                  t <- getType val
                  raise (InvalidType predicate Type.Bool t)

    ReturnStatement {result = Just result} -> do
      cell <- evalExpression result
      pure (Just cell)

    ReturnStatement {result = Nothing} -> do
      cell <- newCell Unit
      pure (Just cell)

    AssertStatement {predicate} -> do
      cell <- evalExpression predicate
      val <- readCell cell

      case val of
        Bool False -> raise (AssertionFailed statement)
        Bool True -> pure Nothing

        _ -> do
          t <- getType val
          raise (InvalidType predicate Type.Bool t)

    BreakpointStatement {} -> do
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


evalExpression :: Expression -> Evaluator Cell
evalExpression expression = case expression of
  IntegerExpression {integer} -> case toIntegralSized integer of
    Just x -> newCell (Int x)
    Nothing -> raise (IntegerOverflow expression)

  RationalExpression {rational} -> newCell (Float (fromRat rational))

  VarExpression {varName, interval} -> lookupVar varName >>= \case
    Just (cell, _) -> pure cell
    Nothing -> raise (UnknownVar varName interval)

  ArrayExpression {elems} -> do
    cells <- flip Vector.unfoldrM elems $ \case
      [] -> pure Nothing

      (elem : elems) -> do
        cell <- evalExpression elem
        cell' <- cloneCell cell
        pure (Just (cell', elems))

    newCell (Array cells)

  AccessExpression {array, index} -> do
    arrayCell <- evalExpression array
    arrayVal <- readCell arrayCell

    indexCell <- evalExpression index
    indexVal <- readCell indexCell

    case (arrayVal, indexVal) of
      (Array cells, Int x) -> case toIntegralSized x of
        Just n | Just cell <- cells !? n -> pure cell
        _ -> raise (IndexOutOfBounds index x)

      (Array _, _) -> do
        indexT <- getType indexVal
        raise (InvalidType index Type.Int indexT)

      (_, _) -> do
        arrayT <- getType arrayVal
        raise (InvalidType array (Type.Array Type.Unknown) arrayT)

  CallExpression {funId = SymbolId {name, interval}, args} -> do
    argCells <- for args evalExpression

    lookupFun name >>= \case
      Just (UserDefined FunDefinition {params, body}, depth) ->
        withNewFrame (Just name) (depth + 1) (go 0 params argCells)

        where
          -- Pass argument by value:
          go n ((Nothing, SymbolId {name}, _) : params) (argCell : argCells) = do
            argCell' <- cloneCell argCell
            defineVar name argCell'
            go (n + 1) params argCells

          -- Pass argument by reference:
          go n ((Just _, SymbolId {name}, _) : params) (argCell : argCells) = do
            defineVar name argCell
            go (n + 1) params argCells

          -- If argument count is correct:
          go _ [] [] = evalStatement body >>= \case
            Just cell -> cloneCell cell
            Nothing -> newCell Unit

          -- If argument count is incorrect:
          go n params argCells = do
            let expected = n + length params
            let actual = n + length argCells
            raise (InvalidArgCount expression expected actual)

      Just (BuiltinNot, _) | [cell] <- argCells -> do
        val <- readCell cell

        case val of
          Bool x -> newCell (Bool (not x))

          _ -> do
            argT <- getType val
            raise (InvalidType (head args) Type.Bool argT)

      Just (BuiltinNot, _) ->
        raise (InvalidArgCount expression 1 (length argCells))

      Just (BuiltinLen, _) | [cell] <- argCells -> do
        val <- readCell cell

        case val of
          Array cells -> newCell (Int (fromIntegral (length cells)))

          _ -> do
            argT <- getType val
            raise (InvalidType (head args) (Type.Array Type.Unknown) argT)

      Just (BuiltinLen, _) ->
        raise (InvalidArgCount expression 1 (length argCells))

      Just (BuiltinToInt, _) | [cell] <- argCells -> do
        val <- readCell cell

        case val of
          Float x -> newCell (Int (round x))

          _ -> do
            argT <- getType val
            raise (InvalidType (head args) Type.Float argT)

      Just (BuiltinToInt, _) ->
        raise (InvalidArgCount expression 1 (length argCells))

      Just (BuiltinToFloat, _) | [cell] <- argCells -> do
        val <- readCell cell

        case val of
          Int x -> newCell (Float (fromIntegral x))

          _ -> do
            argT <- getType val
            raise (InvalidType (head args) Type.Int argT)

      Just (BuiltinToFloat, _) ->
        raise (InvalidArgCount expression 1 (length argCells))

      _ -> raise (UnknownFun name interval)

  UnaryExpression {unary, operand} | PlusOperator {} <- unary -> do
    cell <- evalExpression operand
    val <- readCell cell

    case val of
      Int x -> newCell (Int x)
      Float x -> newCell (Float x)

      _ -> do
        operandT <- getType val
        raise (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | MinusOperator {} <- unary -> do
    cell <- evalExpression operand
    val <- readCell cell

    case val of
      Int x | Just y <- safeUnary negate x -> newCell (Int y)
      Int _ -> raise (IntegerOverflow expression)

      Float x -> newCell (Float (negate x))

      _ -> do
        operandT <- getType val
        raise (InvalidUnary unary operandT)

  BinaryExpression {left, binary, right} | AddOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int x, Int y) | Just z <- safeBinary (+) x y -> newCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newCell (Float (x + y))

      (Array rs1, Array rs2) -> do
        let n1 = Vector.length rs1
        let n2 = Vector.length rs2

        case safeBinary (+) n1 n2 of
          Nothing -> raise (IntegerOverflow expression)

          Just n3 -> do
            let f i = cloneCell (if i < n1 then rs1 ! i else rs2 ! (i - n1))
            cells <- Vector.generateM n3 f
            newCell (Array cells)

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | SubtractOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int x, Int y) | Just z <- safeBinary (-) x y -> newCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newCell (Float (x - y))

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | MultiplyOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int x, Int y) | Just z <- safeBinary (*) x y -> newCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newCell (Float (x * y))

      (Int x, Array _) | x <= 0 -> newCell (Array Vector.empty)
      (Array _, Int y) | y <= 0 -> newCell (Array Vector.empty)

      (Int x, Array cells) -> do
        let n = Vector.length cells

        case safeBinary (*) x n of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            cells' <- Vector.generateM n' (\i -> cloneCell (cells ! (i `mod` n)))
            newCell (Array cells')

      (Array cells, Int y) -> do
        let n = Vector.length cells

        case safeBinary (*) n y of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            cells' <- Vector.generateM n' (\i -> cloneCell (cells ! (i `mod` n)))
            newCell (Array cells')

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | DivideOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary div x y -> newCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> newCell (Float (x / y))

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | ModuloOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary mod x y -> newCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | EqualOperator {} <- binary -> do
    leftCell <- evalExpression left
    rightCell <- evalExpression right

    compareCells leftCell rightCell >>= \case
      Right ordering -> newCell (Bool (ordering == EQ))
      Left (leftT, rightT) -> raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | NotEqualOperator {} <- binary -> do
    leftCell <- evalExpression left
    rightCell <- evalExpression right

    compareCells leftCell rightCell >>= \case
      Right ordering -> newCell (Bool (ordering /= EQ))
      Left (leftT, rightT) -> raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | LessOperator {} <- binary -> do
    leftCell <- evalExpression left
    rightCell <- evalExpression right

    compareCells leftCell rightCell >>= \case
      Right ordering -> newCell (Bool (ordering < EQ))
      Left (leftT, rightT) -> raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | LessOrEqualOperator {} <- binary -> do
    leftCell <- evalExpression left
    rightCell <- evalExpression right

    compareCells leftCell rightCell >>= \case
      Right ordering -> newCell (Bool (ordering <= EQ))
      Left (leftT, rightT) -> raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | GreaterOperator {} <- binary -> do
    leftCell <- evalExpression left
    rightCell <- evalExpression right

    compareCells leftCell rightCell >>= \case
      Right ordering -> newCell (Bool (ordering > EQ))
      Left (leftT, rightT) -> raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | GreaterOrEqualOperator {} <- binary -> do
    leftCell <- evalExpression left
    rightCell <- evalExpression right

    compareCells leftCell rightCell >>= \case
      Right ordering -> newCell (Bool (ordering >= EQ))
      Left (leftT, rightT) -> raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | AndOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    case leftVal of
      Bool False -> newCell (Bool False)

      _ -> do
        rightCell <- evalExpression right
        rightVal <- readCell rightCell

        case (leftVal, rightVal) of
          (Bool x, Bool y) -> newCell (Bool (x && y))

          (_, _) -> do
            leftT <- getType leftVal
            rightT <- getType rightVal
            raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | OrOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    case leftVal of
      Bool True -> newCell (Bool True)

      _ -> do
        rightCell <- evalExpression right
        rightVal <- readCell rightCell

        case (leftVal, rightVal) of
          (Bool x, Bool y) -> newCell (Bool (x || y))

          (_, _) -> do
            leftT <- getType leftVal
            rightT <- getType rightVal
            raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | XorOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Bool x, Bool y) -> newCell (Bool (x /= y))

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary = PlainAssignOperator {}, right} -> do
    leftCell <- evalExpression left

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    rightVal' <- cloneVal rightVal
    writeCell leftCell rightVal'

  BinaryExpression {left, binary, right} | AddAssignOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int x, Int y) | Just z <- safeBinary (+) x y -> writeCell leftCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeCell leftCell (Float (x + y))

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | SubtractAssignOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int x, Int y) | Just z <- safeBinary (-) x y -> writeCell leftCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeCell leftCell (Float (x - y))

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | MultiplyAssignOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int x, Int y) | Just z <- safeBinary (*) x y -> writeCell leftCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeCell leftCell (Float (x * y))

      (Array cells, Int y) -> do
        let n = Vector.length cells

        case safeBinary (*) n y of
          Nothing -> raise (IntegerOverflow expression)

          Just n' -> do
            cells' <- Vector.generateM n' (\i -> cloneCell (cells ! (i `mod` n)))
            writeCell leftCell (Array cells')

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | DivideAssignOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary div x y -> writeCell leftCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (Float x, Float y) -> writeCell leftCell (Float (x / y))

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | ModuloAssignOperator {} <- binary -> do
    leftCell <- evalExpression left
    leftVal <- readCell leftCell

    rightCell <- evalExpression right
    rightVal <- readCell rightCell

    case (leftVal, rightVal) of
      (Int _, Int 0) -> raise (DivisionByZero expression)
      (Int x, Int y) | Just z <- safeBinary mod x y -> writeCell leftCell (Int z)
      (Int _, Int _) -> raise (IntegerOverflow expression)

      (_, _) -> do
        leftT <- getType leftVal
        rightT <- getType rightVal
        raise (InvalidBinary binary leftT rightT)

  ParenthesizedExpression {inner} -> evalExpression inner

  where
    safeUnary op x = toIntegralSized (op (toInteger x))
    safeBinary op x y = toIntegralSized (toInteger x `op` toInteger y)
