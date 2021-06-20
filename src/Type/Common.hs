module Type.Common (
  Type (..),
  Environment (..),
  Error (..),
  label,
  areCompatible,
  description,
  span,
  start,
  end
) where

import Prelude hiding (span)
import Data.List.NonEmpty (NonEmpty)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)

import Span (Span)
import qualified Syntax
import qualified Unicode


data Type where
  Unit :: Type
  Boolean :: Type
  Integer :: Type
  Rational :: Type
  Function :: {parameters :: [Type], result :: Type} -> Type
  Unknown :: {name :: Text} -> Type
  Error :: Type
  deriving (Show, Read)


data Error where
  UnknownTypeError :: {typeId :: Syntax.Identifier ()} -> Error
  UnknownVariableError :: {variableId :: Syntax.Identifier ()} -> Error
  UnknownFunctionError :: {functionId :: Syntax.Identifier (), parameters :: [Type]} -> Error
  FunctionRedefinitionError :: {functionId :: Syntax.Identifier (), parameters :: [Type]} -> Error
  InvalidUnaryError :: {unary :: Syntax.UnaryOperator (), operand :: Type} -> Error
  InvalidBinaryError :: {binary :: Syntax.BinaryOperator (), left :: Type, right :: Type} -> Error
  InvalidAssignError :: {assign :: Syntax.AssignOperator (), target :: Type, value :: Type} -> Error
  InvalidTypeError :: {expression :: Syntax.Expression (), expected :: Type, actual :: Type} -> Error
  NoSideEffectsError :: {statement :: Syntax.Statement ()} -> Error
  InvalidReturnTypeError :: {statement :: Syntax.Statement (), expected :: Type, actual :: Type} -> Error
  MissingReturnValueError :: {statement :: Syntax.Statement (), expected :: Type} -> Error
  MissingReturnPathError :: {functionId :: Syntax.Identifier (), parameters :: [Type]} -> Error
  deriving (Eq, Show, Read)


data Environment where
  Environment :: {
    types :: Map Text Type,
    variables :: Map Text Type,
    functions :: NonEmpty (Map Text [([Type], Type)])
  } -> Environment

  deriving (Eq, Show, Read)


instance Eq Type where
  Unit == Unit = True
  Boolean == Boolean = True
  Integer == Integer = True
  Rational == Rational = True
  Function parameters1 result1 == Function parameters2 result2 = parameters1 == parameters2 && result1 == result2
  Unknown name1 == Unknown name2 = Unicode.collate name1 == Unicode.collate name2
  Error == Error = True
  _ == _ = False


label :: Type -> Text
label Unit = "Unit"
label Boolean = "Boolean"
label Integer = "Integer"
label Rational = "Rational"
label Function{parameters, result} = "(" <> Text.intercalate ", " (label <$> parameters) <> ") → " <> label result
label Unknown{name} = name
label Error = "⊥"


areCompatible :: Type -> Type -> Bool
areCompatible Error  _ = True
areCompatible _ Error = True
areCompatible type1 type2 = type1 == type2


description :: Error -> Text
description = \case
  UnknownTypeError{typeId} ->
    "Unknown type " <> typeId.name

  UnknownVariableError{variableId} ->
    "Unknown variable " <> variableId.name

  UnknownFunctionError{functionId, parameters} ->
    "Unknown function " <> functionId.name <> "(" <> Text.intercalate ", " (label <$> parameters) <> ")"

  FunctionRedefinitionError{functionId, parameters} ->
    "Function " <> functionId.name <> "(" <> Text.intercalate ", " (label <$> parameters) <> ") already defined"

  InvalidUnaryError{unary, operand} ->
    "Can’t apply unary " <> operator <> " to " <> label operand
    where
      operator = case unary of
        Syntax.PlusOperator{} -> "+"
        Syntax.MinusOperator{} -> "-"
        Syntax.NotOperator{} -> "not"

  InvalidBinaryError{binary, left, right} ->
    "Can’t apply binary " <> operator <> " between " <> label left <> " and " <> label right
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

  InvalidAssignError{assign, target, value} ->
    "Can’t apply assign " <> operator <> " to " <> label target <> " and " <> label value
    where
      operator = case assign of
        Syntax.AssignOperator{} -> "="
        Syntax.AddAssignOperator{} -> "+="
        Syntax.SubtractAssignOperator{} -> "-="
        Syntax.MultiplyAssignOperator{} -> "*="
        Syntax.DivideAssignOperator{} -> "/="
        Syntax.RemainderAssignOperator{} -> "%="

  InvalidTypeError{expected, actual} ->
    "Invalid type: expected " <> label expected <> ", but got " <> label actual

  NoSideEffectsError{} ->
    "Statement has no side effects"

  InvalidReturnTypeError{expected, actual}->
    "Invalid return type: expected " <> label expected <> ", but got " <> label actual

  MissingReturnValueError{expected} ->
    "Missing return value: expected " <> label expected

  MissingReturnPathError{functionId, parameters} ->
    functionId.name <> "(" <> Text.intercalate ", " (label <$> parameters) <> "): not all code paths return a value"


span :: Error -> Span
span UnknownTypeError{typeId} = Syntax.span typeId
span UnknownVariableError{variableId} = Syntax.span variableId
span UnknownFunctionError{functionId} = Syntax.span functionId
span FunctionRedefinitionError{functionId} = Syntax.span functionId
span InvalidUnaryError{unary} = Syntax.span unary
span InvalidBinaryError{binary} = Syntax.span binary
span InvalidAssignError{assign} = Syntax.span assign
span InvalidTypeError{expression} = Syntax.span expression
span NoSideEffectsError{statement} = Syntax.span statement
span InvalidReturnTypeError{statement} = Syntax.span statement
span MissingReturnValueError{statement} = Syntax.span statement
span MissingReturnPathError{functionId} = Syntax.span functionId


start :: Integral a => Error -> a
start UnknownTypeError{typeId} = Syntax.start typeId
start UnknownVariableError{variableId} = Syntax.start variableId
start UnknownFunctionError{functionId} = Syntax.start functionId
start FunctionRedefinitionError{functionId} = Syntax.start functionId
start InvalidUnaryError{unary} = Syntax.start unary
start InvalidBinaryError{binary} = Syntax.start binary
start InvalidAssignError{assign} = Syntax.start assign
start InvalidTypeError{expression} = Syntax.start expression
start NoSideEffectsError{statement} = Syntax.start statement
start InvalidReturnTypeError{statement} = Syntax.start statement
start MissingReturnValueError{statement} = Syntax.start statement
start MissingReturnPathError{functionId} = Syntax.start functionId


end :: Integral a => Error -> a
end UnknownTypeError{typeId} = Syntax.end typeId
end UnknownVariableError{variableId} = Syntax.end variableId
end UnknownFunctionError{functionId} = Syntax.end functionId
end FunctionRedefinitionError{functionId} = Syntax.end functionId
end InvalidUnaryError{unary} = Syntax.end unary
end InvalidBinaryError{binary} = Syntax.end binary
end InvalidAssignError{assign} = Syntax.end assign
end InvalidTypeError{expression} = Syntax.end expression
end NoSideEffectsError{statement} = Syntax.end statement
end InvalidReturnTypeError{statement} = Syntax.end statement
end MissingReturnValueError{statement} = Syntax.end statement
end MissingReturnPathError{functionId} = Syntax.end functionId
