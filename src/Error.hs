module Error (
  Error (..),
  description
) where

import Prelude hiding (span)

import Data.Text (Text)
import qualified Data.Text as Text

import Span (Span)
import qualified Span
import qualified Syntax
import Type (Type)
import qualified Type


data Error where
  UnknownType :: {typeId :: Syntax.Identifier} -> Error
  UnknownVariable :: {variableId :: Syntax.Identifier} -> Error
  UnknownFunction :: {functionId :: Syntax.Identifier, parameters :: [Type]} -> Error
  FunctionRedefinition :: {functionId :: Syntax.Identifier, parameters :: [Type]} -> Error
  InvalidUnary :: {unary :: Syntax.UnaryOperator, operand :: Type} -> Error
  InvalidBinary :: {binary :: Syntax.BinaryOperator, left :: Type, right :: Type} -> Error
  InvalidAssign :: {assign :: Syntax.AssignOperator, target :: Type, value :: Type} -> Error
  InvalidType :: {expression :: Syntax.Expression, expected :: Type, actual :: Type} -> Error
  NoSideEffects :: {statement :: Syntax.Statement} -> Error
  InvalidReturnType :: {statement :: Syntax.Statement, expected :: Type, actual :: Type} -> Error
  MissingReturnValue :: {statement :: Syntax.Statement, expected :: Type} -> Error
  MissingReturnPath :: {functionId :: Syntax.Identifier, parameters :: [Type]} -> Error
  deriving (Eq, Show, Read)


description :: Error -> Text
description = \case
  UnknownType{typeId} ->
    "Unknown type " <> typeId.name

  UnknownVariable{variableId} ->
    "Unknown variable " <> variableId.name

  UnknownFunction{functionId, parameters} ->
    let parameterList = "(" <> Text.intercalate ", " (Type.label <$> parameters) <> ")"
     in "Unknown function " <> functionId.name <> parameterList

  FunctionRedefinition{functionId, parameters} ->
    let parameterList = "(" <> Text.intercalate ", " (Type.label <$> parameters) <> ")"
     in "Function " <> functionId.name <> parameterList <> "already defined"

  InvalidUnary{unary, operand} ->
    "Can’t apply unary " <> operator <> " to " <> Type.label operand
    where
      operator = case unary of
        Syntax.PlusOperator{} -> "+"
        Syntax.MinusOperator{} -> "-"
        Syntax.NotOperator{} -> "not"

  InvalidBinary{binary, left, right} ->
    "Can’t apply binary " <> operator <> " between " <> Type.label left <> " and " <> Type.label right
    where
      operator = case binary of
        Syntax.AddOperator{} -> "+"
        Syntax.SubtractOperator{} -> "-"
        Syntax.MultiplyOperator{} -> "*"
        Syntax.DivideOperator{} -> "/"
        Syntax.RemainderOperator{} -> "%"
        Syntax.EqualOperator{} -> "=="
        Syntax.NotEqualOperator{} -> "!="
        Syntax.LessOperator{} -> "<"
        Syntax.LessOrEqualOperator{} -> "<="
        Syntax.GreaterOperator{} -> ">"
        Syntax.GreaterOrEqualOperator{} -> ">="
        Syntax.AndOperator{} -> "and"
        Syntax.OrOperator{} -> "or"

  InvalidAssign{assign, target, value} ->
    "Can’t apply assign " <> operator <> " to " <> Type.label target <> " and " <> Type.label value
    where
      operator = case assign of
        Syntax.AssignOperator{} -> "="
        Syntax.AddAssignOperator{} -> "+="
        Syntax.SubtractAssignOperator{} -> "-="
        Syntax.MultiplyAssignOperator{} -> "*="
        Syntax.DivideAssignOperator{} -> "/="
        Syntax.RemainderAssignOperator{} -> "%="

  InvalidType{expected, actual} ->
    "Invalid type: expected " <> Type.label expected <> ", but got " <> Type.label actual

  NoSideEffects{} ->
    "Statement has no side effects"

  InvalidReturnType{expected, actual}->
    "Invalid return type: expected " <> Type.label expected <> ", but got " <> Type.label actual

  MissingReturnValue{expected} ->
    "Missing return value: expected " <> Type.label expected

  MissingReturnPath{functionId, parameters} ->
    let parameterList = "(" <> Text.intercalate ", " (Type.label <$> parameters) <> ")"
     in functionId.name <> parameterList <> ": not all code paths return a value"


instance Span Error where
  start UnknownType{typeId} = Span.start typeId
  start UnknownVariable{variableId} = Span.start variableId
  start UnknownFunction{functionId} = Span.start functionId
  start FunctionRedefinition{functionId} = Span.start functionId
  start InvalidUnary{unary} = Span.start unary
  start InvalidBinary{binary} = Span.start binary
  start InvalidAssign{assign} = Span.start assign
  start InvalidType{expression} = Span.start expression
  start NoSideEffects{statement} = Span.start statement
  start InvalidReturnType{statement} = Span.start statement
  start MissingReturnValue{statement} = Span.start statement
  start MissingReturnPath{functionId} = Span.start functionId

  end UnknownType{typeId} = Span.end typeId
  end UnknownVariable{variableId} = Span.end variableId
  end UnknownFunction{functionId} = Span.end functionId
  end FunctionRedefinition{functionId} = Span.end functionId
  end InvalidUnary{unary} = Span.end unary
  end InvalidBinary{binary} = Span.end binary
  end InvalidAssign{assign} = Span.end assign
  end InvalidType{expression} = Span.end expression
  end NoSideEffects{statement} = Span.end statement
  end InvalidReturnType{statement} = Span.end statement
  end MissingReturnValue{statement} = Span.end statement
  end MissingReturnPath{functionId} = Span.end functionId
