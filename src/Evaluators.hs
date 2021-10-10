module Evaluators (
  evaluateDeclaration,
  evaluateStatement,
  evaluateExpression
) where

import Control.Monad
import Data.Maybe
import Data.Traversable

import qualified CallTarget
import Evaluator (EvaluatorT)
import qualified Evaluator
import Evaluator.Value (Value)
import qualified Evaluator.Value as Value
import Evaluator.Variable (Variable)
import qualified Evaluator.Variable as Variable
import qualified Syntax


evaluateDeclaration :: MonadFail m => Syntax.Declaration -> EvaluatorT m ()
evaluateDeclaration = \case
  Syntax.VariableDeclaration{variableId, right} -> do
    value <- evaluateExpression right
    Evaluator.setVariable variableId.name (Variable.ByValue value)

  Syntax.FunctionDeclaration{} -> pure ()


evaluateStatement :: MonadFail m => Syntax.Statement -> EvaluatorT m (Maybe Value)
evaluateStatement statement = case statement of
  Syntax.DeclarationStatement{declaration} -> do
    evaluateDeclaration declaration
    pure Nothing

  Syntax.ExpressionStatement{expression} -> do
    configuration <- Evaluator.getConfiguration
    configuration.beforeStatement
    evaluateExpression expression
    configuration.afterStatement
    pure Nothing

  Syntax.IfStatement{predicate, trueBranch} -> do
    configuration <- Evaluator.getConfiguration
    configuration.beforePredicate
    Value.Bool predicateValue <- evaluateExpression predicate
    configuration.afterPredicate
    when predicateValue (void (Evaluator.push (evaluateStatement trueBranch)))
    pure Nothing

  Syntax.IfElseStatement{predicate, trueBranch, falseBranch} -> do
    configuration <- Evaluator.getConfiguration
    configuration.beforePredicate
    Value.Bool predicateValue <- evaluateExpression predicate
    configuration.afterPredicate

    if predicateValue then
      Evaluator.push (evaluateStatement trueBranch)
    else
      Evaluator.push (evaluateStatement falseBranch)

  Syntax.WhileStatement{predicate, body} -> do
    configuration <- Evaluator.getConfiguration
    configuration.beforePredicate
    Value.Bool predicateValue <- evaluateExpression predicate
    configuration.afterPredicate
    when predicateValue (void (Evaluator.push (evaluateStatement body *> evaluateStatement statement)))
    pure Nothing

  Syntax.DoWhileStatement{body, predicate} -> do
    configuration <- Evaluator.getConfiguration
    evaluateStatement body
    configuration.beforePredicate
    Value.Bool predicateValue <- evaluateExpression predicate
    configuration.afterPredicate
    when predicateValue (void (Evaluator.push (evaluateStatement statement)))
    pure Nothing

  Syntax.ReturnStatement{result = Just result} -> do
    configuration <- Evaluator.getConfiguration
    configuration.beforeStatement
    value <- evaluateExpression result
    configuration.afterStatement
    pure (Just value)

  Syntax.ReturnStatement{result = Nothing} -> do
    configuration <- Evaluator.getConfiguration
    configuration.beforeStatement
    configuration.afterStatement
    pure (Just Value.Unit)

  Syntax.BlockStatement{statements} -> Evaluator.push (go statements)
    where
      go [] = pure Nothing

      go (statement : statements) = evaluateStatement statement >>= \case
        Just value -> pure (Just value)
        Nothing -> go statements


evaluateExpression :: MonadFail m => Syntax.Expression -> EvaluatorT m Value
evaluateExpression = \case
  Syntax.IntegerExpression{integer} -> pure (Value.Int (fromInteger integer))

  Syntax.RationalExpression{rational} -> pure (Value.Float (fromRational rational))

  Syntax.VariableExpression{variableId} ->
    getValue =<< Evaluator.getVariable variableId.name

  Syntax.CallExpression{arguments, depth, target} -> do
    values <- for arguments evaluateExpression

    case (target, values) of
      (CallTarget.IntToInt, value : _) -> pure value

      (CallTarget.FloatToInt, Value.Float value : _) -> pure (Value.Int (round value))

      (CallTarget.IntToFloat, Value.Int value : _) -> pure (Value.Float (fromIntegral value))

      (CallTarget.FloatToFloat, value : _) -> pure value

      (CallTarget.UserDefined depth' parameters body, _) ->
        Evaluator.pop (depth - depth') $ Evaluator.push do
          zipWithM_ (\p v -> Evaluator.setVariable p.name (Variable.ByValue v)) parameters values
          fromJust <$> evaluateStatement body

      _ -> undefined

  Syntax.UnaryExpression{unary, operand} -> do
    operandValue <- evaluateExpression operand

    pure case (unary, operandValue) of
      (Syntax.PlusOperator{}, _) -> operandValue
      (Syntax.MinusOperator{}, Value.Int value) -> Value.Int (- value)
      (Syntax.MinusOperator{}, Value.Float value) -> Value.Float (- value)
      (Syntax.NotOperator{}, Value.Bool value) -> Value.Bool (not value)
      _ -> undefined

  Syntax.BinaryExpression{left, binary, right} -> do
    leftValue <- evaluateExpression left
    rightValue <- evaluateExpression right

    pure case (leftValue, binary, rightValue) of
      (Value.Int left, Syntax.AddOperator{}, Value.Int right) -> Value.Int (left + right)
      (Value.Float left, Syntax.AddOperator{}, Value.Float right) -> Value.Float (left + right)
      (Value.Int left, Syntax.SubtractOperator{}, Value.Int right) -> Value.Int (left - right)
      (Value.Float left, Syntax.SubtractOperator{}, Value.Float right) -> Value.Float (left - right)
      (Value.Int left, Syntax.MultiplyOperator{}, Value.Int right) -> Value.Int (left * right)
      (Value.Float left, Syntax.MultiplyOperator{}, Value.Float right) -> Value.Float (left * right)
      (Value.Int left, Syntax.DivideOperator{}, Value.Int right) -> Value.Int (left `div` right)
      (Value.Float left, Syntax.DivideOperator{}, Value.Float right) -> Value.Float (left / right)
      (Value.Int left, Syntax.ModuloOperator{}, Value.Int right) -> Value.Int (left `mod` right)
      (Value.Unit, Syntax.EqualOperator{}, Value.Unit) -> Value.Bool True
      (Value.Int left, Syntax.EqualOperator{}, Value.Int right) -> Value.Bool (left == right)
      (Value.Float left, Syntax.EqualOperator{}, Value.Float right) -> Value.Bool (left == right)
      (_, Syntax.EqualOperator{}, _) -> Value.Bool False
      (Value.Unit, Syntax.NotEqualOperator{}, Value.Unit) -> Value.Bool False
      (Value.Int left, Syntax.NotEqualOperator{}, Value.Int right) -> Value.Bool (left /= right)
      (Value.Float left, Syntax.NotEqualOperator{}, Value.Float right) -> Value.Bool (left /= right)
      (_, Syntax.NotEqualOperator{}, _) -> Value.Bool True
      (Value.Int left, Syntax.LessOperator{}, Value.Int right) -> Value.Bool (left < right)
      (Value.Float left, Syntax.LessOperator{}, Value.Float right) -> Value.Bool (left < right)
      (Value.Int left, Syntax.LessOrEqualOperator{}, Value.Int right) -> Value.Bool (left <= right)
      (Value.Float left, Syntax.LessOrEqualOperator{}, Value.Float right) -> Value.Bool (left <= right)
      (Value.Int left, Syntax.GreaterOperator{}, Value.Int right) -> Value.Bool (left > right)
      (Value.Float left, Syntax.GreaterOperator{}, Value.Float right) -> Value.Bool (left > right)
      (Value.Int left, Syntax.GreaterOrEqualOperator{}, Value.Int right) -> Value.Bool (left >= right)
      (Value.Float left, Syntax.GreaterOrEqualOperator{}, Value.Float right) -> Value.Bool (left >= right)
      (Value.Bool left, Syntax.AndOperator{}, Value.Bool right) -> Value.Bool (left && right)
      (Value.Bool left, Syntax.OrOperator{}, Value.Bool right) -> Value.Bool (left || right)
      _ -> undefined

  Syntax.AssignExpression{variableId, assign, right} -> do
    value <- evaluateExpression right
    variable <- Evaluator.getVariable variableId.name

    value' <- case assign of
      Syntax.AssignOperator{} -> pure value

      _ -> do
        variableValue <- getValue variable

        pure case (value, assign, variableValue) of
          (Value.Int left, Syntax.AddAssignOperator{}, Value.Int right) -> Value.Int (left + right)
          (Value.Float left, Syntax.AddAssignOperator{}, Value.Float right) -> Value.Float (left + right)
          (Value.Int left, Syntax.SubtractAssignOperator{}, Value.Int right) -> Value.Int (left - right)
          (Value.Float left, Syntax.SubtractAssignOperator{}, Value.Float right) -> Value.Float (left - right)
          (Value.Int left, Syntax.MultiplyAssignOperator{}, Value.Int right) -> Value.Int (left * right)
          (Value.Float left, Syntax.MultiplyAssignOperator{}, Value.Float right) -> Value.Float (left * right)
          (Value.Int left, Syntax.DivideAssignOperator{}, Value.Int right) -> Value.Int (left `div` right)
          (Value.Float left, Syntax.DivideAssignOperator{}, Value.Float right) -> Value.Float (left / right)
          (Value.Int left, Syntax.ModuloAssignOperator{}, Value.Int right) -> Value.Int (left `mod` right)
          _ -> undefined

    case variable of
      Variable.ByValue _ -> Evaluator.setVariable variableId.name (Variable.ByValue value')
      Variable.ByReference name -> Evaluator.setVariable name (Variable.ByValue value')

    pure value'

  Syntax.ParenthesizedExpression{inner} -> evaluateExpression inner


getValue :: Monad m => Variable -> EvaluatorT m Value
getValue (Variable.ByValue value) = pure value
getValue (Variable.ByReference name) = getValue =<< Evaluator.getVariable name
