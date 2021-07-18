module Error (
  Error (..),
  description,
  span,
  start,
  end
) where

import Prelude hiding (span)

import Data.Text (Text)
import qualified Data.Text as Text

import Span (Span)
import qualified Syntax
import Type (Type)
import qualified Type


data Error where
  UnknownType :: {typeId :: Syntax.Identifier ()} -> Error
  UnknownVariable :: {variableId :: Syntax.Identifier ()} -> Error
  UnknownFunction :: {functionId :: Syntax.Identifier (), parameters :: [Type]} -> Error
  FunctionRedefinition :: {functionId :: Syntax.Identifier (), parameters :: [Type]} -> Error
  InvalidUnary :: {unary :: Syntax.UnaryOperator (), operand :: Type} -> Error
  InvalidBinary :: {binary :: Syntax.BinaryOperator (), left :: Type, right :: Type} -> Error
  InvalidAssign :: {assign :: Syntax.AssignOperator (), target :: Type, value :: Type} -> Error
  InvalidType :: {expression :: Syntax.Expression (), expected :: Type, actual :: Type} -> Error
  NoSideEffects :: {statement :: Syntax.Statement ()} -> Error
  InvalidReturnType :: {statement :: Syntax.Statement (), expected :: Type, actual :: Type} -> Error
  MissingReturnValue :: {statement :: Syntax.Statement (), expected :: Type} -> Error
  MissingReturnPath :: {functionId :: Syntax.Identifier (), parameters :: [Type]} -> Error
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


span :: Error -> Span
span UnknownType{typeId} = Syntax.span typeId
span UnknownVariable{variableId} = Syntax.span variableId
span UnknownFunction{functionId} = Syntax.span functionId
span FunctionRedefinition{functionId} = Syntax.span functionId
span InvalidUnary{unary} = Syntax.span unary
span InvalidBinary{binary} = Syntax.span binary
span InvalidAssign{assign} = Syntax.span assign
span InvalidType{expression} = Syntax.span expression
span NoSideEffects{statement} = Syntax.span statement
span InvalidReturnType{statement} = Syntax.span statement
span MissingReturnValue{statement} = Syntax.span statement
span MissingReturnPath{functionId} = Syntax.span functionId


start :: Num a => Error -> a
start UnknownType{typeId} = Syntax.start typeId
start UnknownVariable{variableId} = Syntax.start variableId
start UnknownFunction{functionId} = Syntax.start functionId
start FunctionRedefinition{functionId} = Syntax.start functionId
start InvalidUnary{unary} = Syntax.start unary
start InvalidBinary{binary} = Syntax.start binary
start InvalidAssign{assign} = Syntax.start assign
start InvalidType{expression} = Syntax.start expression
start NoSideEffects{statement} = Syntax.start statement
start InvalidReturnType{statement} = Syntax.start statement
start MissingReturnValue{statement} = Syntax.start statement
start MissingReturnPath{functionId} = Syntax.start functionId


end :: Num a => Error -> a
end UnknownType{typeId} = Syntax.end typeId
end UnknownVariable{variableId} = Syntax.end variableId
end UnknownFunction{functionId} = Syntax.end functionId
end FunctionRedefinition{functionId} = Syntax.end functionId
end InvalidUnary{unary} = Syntax.end unary
end InvalidBinary{binary} = Syntax.end binary
end InvalidAssign{assign} = Syntax.end assign
end InvalidType{expression} = Syntax.end expression
end NoSideEffects{statement} = Syntax.end statement
end InvalidReturnType{statement} = Syntax.end statement
end MissingReturnValue{statement} = Syntax.end statement
end MissingReturnPath{functionId} = Syntax.end functionId
