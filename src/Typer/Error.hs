module Typer.Error (
  Error (..),
  description
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Range
import Syntax
import Type


data Error where
  UnknownType :: {typeId :: Identifier} -> Error
  UnknownVariable :: {variableId :: Identifier} -> Error
  UnknownFunction :: {functionId :: Identifier, parameters :: [Type]} -> Error
  FunctionRedefinition :: {functionId :: Identifier, parameters :: [Type]} -> Error
  InvalidUnary :: {unary :: UnaryOperator, operand :: Type} -> Error
  InvalidBinary :: {binary :: BinaryOperator, left :: Type, right :: Type} -> Error
  InvalidAssign :: {assign :: AssignOperator, target :: Type, value :: Type} -> Error
  InvalidType :: {expression :: Expression, expected :: Type, actual :: Type} -> Error
  InvalidReturnType :: {statement :: Statement, expected :: Type, actual :: Type} -> Error
  MissingReturnValue :: {statement :: Statement, expected :: Type} -> Error
  MissingReturnPath :: {functionId :: Identifier, parameters :: [Type]} -> Error
  deriving (Eq, Show, Read)


instance Range Error where
  start UnknownType{typeId} = start typeId
  start UnknownVariable{variableId} = start variableId
  start UnknownFunction{functionId} = start functionId
  start FunctionRedefinition{functionId} = start functionId
  start InvalidUnary{unary} = start unary
  start InvalidBinary{binary} = start binary
  start InvalidAssign{assign} = start assign
  start InvalidType{expression} = start expression
  start InvalidReturnType{statement} = start statement
  start MissingReturnValue{statement} = start statement
  start MissingReturnPath{functionId} = start functionId

  end UnknownType{typeId} = end typeId
  end UnknownVariable{variableId} = end variableId
  end UnknownFunction{functionId} = end functionId
  end FunctionRedefinition{functionId} = end functionId
  end InvalidUnary{unary} = end unary
  end InvalidBinary{binary} = end binary
  end InvalidAssign{assign} = end assign
  end InvalidType{expression} = end expression
  end InvalidReturnType{statement} = end statement
  end MissingReturnValue{statement} = end statement
  end MissingReturnPath{functionId} = end functionId


description :: Error -> Text
description error = case error of
  UnknownType{typeId} ->
    "Unknown type " <> typeId.name

  UnknownVariable{variableId} ->
    "Unknown variable " <> variableId.name

  UnknownFunction{functionId, parameters} -> do
    let parameterList = "(" <> Text.intercalate ", " (pretty <$> parameters) <> ")"
    "Unknown function " <> functionId.name <> parameterList

  FunctionRedefinition{functionId, parameters} -> do
    let parameterList = "(" <> Text.intercalate ", " (pretty <$> parameters) <> ")"
    "Function " <> functionId.name <> parameterList <> "already defined"

  InvalidUnary{unary, operand} ->
    "Can’t apply unary " <> operator <> " to " <> pretty operand
    where
      operator = case unary of
        PlusOperator{} -> "+"
        MinusOperator{} -> "-"
        NotOperator{} -> "not"

  InvalidBinary{binary, left, right} ->
    "Can’t apply binary " <> operator <> " between " <> pretty left <> " and " <> pretty right
    where
      operator = case binary of
        AddOperator{} -> "+"
        SubtractOperator{} -> "-"
        MultiplyOperator{} -> "*"
        DivideOperator{} -> "/"
        ModuloOperator{} -> "%"
        EqualOperator{} -> "=="
        NotEqualOperator{} -> "!="
        LessOperator{} -> "<"
        LessOrEqualOperator{} -> "<="
        GreaterOperator{} -> ">"
        GreaterOrEqualOperator{} -> ">="
        AndOperator{} -> "and"
        OrOperator{} -> "or"

  InvalidAssign{assign, target, value} ->
    "Can’t apply assign " <> operator <> " to " <> pretty target <> " and " <> pretty value
    where
      operator = case assign of
        AssignOperator{} -> "="
        AddAssignOperator{} -> "+="
        SubtractAssignOperator{} -> "-="
        MultiplyAssignOperator{} -> "*="
        DivideAssignOperator{} -> "/="
        ModuloAssignOperator{} -> "%="

  InvalidType{expected, actual} ->
    "Invalid type: expected " <> pretty expected <> ", but got " <> pretty actual

  InvalidReturnType{expected, actual} ->
    "Invalid return type: expected " <> pretty expected <> ", but got " <> pretty actual

  MissingReturnValue{expected} ->
    "Missing return value: expected " <> pretty expected

  MissingReturnPath{functionId, parameters} -> do
    let parameterList = "(" <> Text.intercalate ", " (pretty <$> parameters) <> ")"
    functionId.name <> parameterList <> ": not all code paths return a value"
