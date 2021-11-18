module Devin.Evaluators (
  evaluateDevin,
  evaluateDeclaration,
  evaluateStatement,
  evaluateExpression
) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Traversable

import Control.Monad.Extra

import qualified Devin.CallTarget as CallTarget
import Devin.Evaluator
import Devin.Range
import Devin.Syntax
import Devin.Value


evaluateDevin :: Monad m => Devin -> Evaluator m ()
evaluateDevin devin = void $ push 1 $ do
  for_ devin.declarations evaluateDeclaration
  push 1 (evaluateStatement (fromJust (findDeclaration f devin)).body)
  where
    f FunctionDeclaration{functionId} | functionId.name == "main" = True
    f _ = False


evaluateDeclaration :: Monad m => Declaration -> Evaluator m ()
evaluateDeclaration declaration = case declaration of
  VariableDeclaration{variableId, right} -> do
    configuration <- getConfiguration
    configuration.beforeExpression right
    value <- evaluateExpression right
    configuration.afterExpression right
    defineVariable variableId.name value

  FunctionDeclaration{} -> pure ()


evaluateStatement :: Monad m => Statement -> Evaluator m (Maybe Value)
evaluateStatement statement = case statement of
  DeclarationStatement{declaration} -> do
    evaluateDeclaration declaration
    pure Nothing

  ExpressionStatement{value} -> do
    configuration <- getConfiguration
    configuration.beforeStatement statement
    evaluateExpression value
    configuration.afterStatement statement
    pure Nothing

  IfStatement{predicate, trueBranch} -> do
    configuration <- getConfiguration
    configuration.beforeExpression predicate
    predicateValue <- evaluateExpression predicate
    configuration.afterExpression predicate

    case predicateValue of
      Bool True -> push 1 (evaluateStatement trueBranch)
      Bool False -> pure Nothing
      _ -> undefined

  IfElseStatement{predicate, trueBranch, falseBranch} -> do
    configuration <- getConfiguration
    configuration.beforeExpression predicate
    predicateValue <- evaluateExpression predicate
    configuration.afterExpression predicate

    case predicateValue of
      Bool True -> push 1 (evaluateStatement trueBranch)
      Bool False -> push 1 (evaluateStatement falseBranch)
      _ -> undefined

  WhileStatement{predicate, body} -> do
    configuration <- getConfiguration
    configuration.beforeExpression predicate
    predicateValue <- evaluateExpression predicate
    configuration.afterExpression predicate

    case predicateValue of
      Bool True -> do
        push 1 (evaluateStatement body)
        evaluateStatement statement

      Bool False -> pure Nothing

      _ -> undefined

  DoWhileStatement{body, predicate} -> do
    value <- push 1 (evaluateStatement body)

    case value of
      Just value -> pure (Just value)

      Nothing -> do
        configuration <- getConfiguration
        configuration.beforeExpression predicate
        predicateValue <- evaluateExpression predicate
        configuration.afterExpression predicate

        case predicateValue of
          Bool True -> evaluateStatement statement
          Bool False -> pure Nothing
          _ -> undefined

  ReturnStatement{result = Just result} -> do
    configuration <- getConfiguration
    configuration.beforeStatement statement
    value <- evaluateExpression result
    configuration.afterStatement statement
    pure (Just value)

  ReturnStatement{result = Nothing} -> do
    configuration <- getConfiguration
    configuration.beforeStatement statement
    configuration.afterStatement statement
    pure (Just Unit)

  BlockStatement{statements} ->
    push 1 (firstJustM evaluateStatement statements)


evaluateExpression :: Monad m => Expression -> Evaluator m Value
evaluateExpression expression = case expression of
  IntegerExpression{integer} -> pure (Int (fromInteger integer))

  RationalExpression{rational} -> pure (Float (fromRational rational))

  VariableExpression{variableId} -> getVariable variableId.name

  CallExpression{arguments, depth, target} -> do
    values <- for arguments evaluateExpression

    case (target, values) of
      (CallTarget.IntToInt, Int x : _) -> pure (Int x)

      (CallTarget.FloatToInt, Float x : _) -> pure (Int (round x))

      (CallTarget.IntToFloat, Int x : _) -> pure (Float (fromIntegral x))

      (CallTarget.FloatToFloat, Float x : _) -> pure (Float x)

      (CallTarget.UserDefined position depth', _) ->
        push (depth - depth' + 1) $ do
          root <- getRoot

          case findDeclaration (\d -> start d == position) root of
            Just FunctionDeclaration{body, parameters} -> do
              zipWithM_ (\p -> defineVariable p._1.name) parameters values
              value <- evaluateStatement body
              pure (fromMaybe Unit value)

            _ -> undefined

      _ -> undefined

  UnaryExpression{unary, operand} -> do
    operandValue <- evaluateExpression operand

    case (unary, operandValue) of
      (PlusOperator{}, Int x) -> pure (Int x)
      (PlusOperator{}, Float x) -> pure (Float x)
      (MinusOperator{}, Int x) -> pure (Int (negate x))
      (MinusOperator{}, Float x) -> pure (Float (negate x))
      (NotOperator{}, Bool x) -> pure (Bool (not x))
      _ -> undefined

  BinaryExpression{left, binary, right} -> do
    leftValue <- evaluateExpression left
    rightValue <- evaluateExpression right

    case (leftValue, binary, rightValue) of
      (Int x, AddOperator{}, Int y) -> pure (Int (x + y))
      (Float x, AddOperator{}, Float y) -> pure (Float (x + y))
      (Int x, SubtractOperator{}, Int y) -> pure (Int (x - y))
      (Float x, SubtractOperator{}, Float y) -> pure (Float (x - y))
      (Int x, MultiplyOperator{}, Int y) -> pure (Int (x * y))
      (Float x, MultiplyOperator{}, Float y) -> pure (Float (x * y))
      (Int x, DivideOperator{}, Int y) -> pure (Int (x `div` y))
      (Float x, DivideOperator{}, Float y) -> pure (Float (x / y))
      (Int x, ModuloOperator{}, Int y) -> pure (Int (x `mod` y))
      (Unit, EqualOperator{}, Unit) -> pure (Bool True)
      (Int x, EqualOperator{}, Int y) -> pure (Bool (x == y))
      (Float x, EqualOperator{}, Float y) -> pure (Bool (x == y))
      (_, EqualOperator{}, _) -> pure (Bool False)
      (Unit, NotEqualOperator{}, Unit) -> pure (Bool False)
      (Int x, NotEqualOperator{}, Int y) -> pure (Bool (x /= y))
      (Float x, NotEqualOperator{}, Float y) -> pure (Bool (x /= y))
      (_, NotEqualOperator{}, _) -> pure (Bool True)
      (Int x, LessOperator{}, Int y) -> pure (Bool (x < y))
      (Float x, LessOperator{}, Float y) -> pure (Bool (x < y))
      (Int x, LessOrEqualOperator{}, Int y) -> pure (Bool (x <= y))
      (Float x, LessOrEqualOperator{}, Float y) -> pure (Bool (x <= y))
      (Int x, GreaterOperator{}, Int y) -> pure (Bool (x > y))
      (Float x, GreaterOperator{}, Float y) -> pure (Bool (x > y))
      (Int x, GreaterOrEqualOperator{}, Int y) -> pure (Bool (x >= y))
      (Float x, GreaterOrEqualOperator{}, Float y) -> pure (Bool (x >= y))
      (Bool x, AndOperator{}, Bool y) -> pure (Bool (x && y))
      (Bool x, OrOperator{}, Bool y) -> pure (Bool (x || y))
      _ -> undefined

  AssignExpression{variableId, assign, right} -> do
    rightValue <- evaluateExpression right

    value <- case assign of
      AssignOperator{} -> pure rightValue

      _ -> do
        leftValue <- getVariable variableId.name

        case (leftValue, assign, rightValue) of
          (Int x, AddAssignOperator{}, Int y) -> pure (Int (x + y))
          (Float x, AddAssignOperator{}, Float y) -> pure (Float (x + y))
          (Int x, SubtractAssignOperator{}, Int y) -> pure (Int (x - y))
          (Float x, SubtractAssignOperator{}, Float y) -> pure (Float (x - y))
          (Int x, MultiplyAssignOperator{}, Int y) -> pure (Int (x * y))
          (Float x, MultiplyAssignOperator{}, Float y) -> pure (Float (x * y))
          (Int x, DivideAssignOperator{}, Int y) -> pure (Int (x `div` y))
          (Float x, DivideAssignOperator{}, Float y) -> pure (Float (x / y))
          (Int x, ModuloAssignOperator{}, Int y) -> pure (Int (x `mod` y))
          _ -> undefined

    setVariable variableId.name value
    pure value

  ParenthesizedExpression{inner} -> evaluateExpression inner
