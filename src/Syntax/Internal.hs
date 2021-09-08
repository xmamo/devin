module Syntax.Internal (
  Node (..),
  Declaration (..),
  Statement (..),
  Expression (..),
  UnaryOperator (..),
  BinaryOperator (..),
  AssignOperator (..),
  Identifier (..),
  Token (..),
  Comment (..),
  CallTarget (..),
  doesReturn,
  hasSideEffects,
  comparePrecedence
) where

import Prelude hiding (span)
import Data.Ord

import Data.Text (Text)

import Span (Span)
import qualified Span
import Type (Type)


class Span a => Node a where
  label :: a -> Text

  isLeaf :: a -> Bool


data Declaration where
  VariableDeclaration :: {
    varKeyword :: Token,
    variableId :: Identifier,
    typeInfo :: Maybe (Token, Identifier),
    equalSign :: Token,
    value :: Expression,
    semicolon :: Token
  } -> Declaration

  FunctionDeclaration :: {
    defKeyword :: Token,
    functionId :: Identifier,
    open :: Token,
    parameters :: Maybe (Identifier, Token, Identifier, [(Token, Identifier, Token, Identifier)]),
    close :: Token,
    returnInfo :: Maybe (Token, Identifier),
    body :: Statement
  } -> Declaration

  deriving (Eq, Show, Read)


data Statement where
  ExpressionStatement :: {
    value :: Expression,
    semicolon :: Token
  } -> Statement

  IfStatement :: {
    ifKeyword :: Token,
    predicate :: Expression,
    trueBranch :: Statement
  } -> Statement

  IfElseStatement :: {
    ifKeyword :: Token,
    predicate :: Expression,
    trueBranch :: Statement,
    elseKeyword :: Token,
    falseBranch :: Statement
  } -> Statement

  WhileStatement :: {
    whileKeyword :: Token,
    predicate :: Expression,
    body :: Statement
  } -> Statement

  DoWhileStatement :: {
    doKeyword :: Token,
    body :: Statement,
    whileKeyword :: Token,
    predicate :: Expression,
    semicolon :: Token
  } -> Statement

  ReturnStatement :: {
    returnKeyword :: Token,
    result :: Maybe Expression,
    semicolon :: Token
  } -> Statement

  BlockStatement :: {
    open :: Token,
    elements :: [Either Declaration Statement],
    close :: Token
  } -> Statement

  deriving (Eq, Show, Read)


data Expression where
  IntegerExpression :: {
    span :: (Int, Int),
    integer :: Integer,
    t :: Type
  } -> Expression

  RationalExpression :: {
    span :: (Int, Int),
    rational :: Rational,
    t :: Type
  } -> Expression

  VariableExpression :: {
    variableId :: Identifier,
    t :: Type
  } -> Expression

  CallExpression :: {
    targetId :: Identifier,
    open :: Token,
    arguments :: Maybe (Expression, [(Token, Expression)]),
    close :: Token,
    target :: CallTarget,
    t :: Type
  } -> Expression

  UnaryExpression :: {
    unary :: UnaryOperator,
    operand :: Expression,
    t :: Type
  } -> Expression

  BinaryExpression :: {
    left :: Expression,
    binary :: BinaryOperator,
    right :: Expression,
    t :: Type
  } -> Expression

  AssignExpression :: {
    targetId :: Identifier,
    assign :: AssignOperator,
    value :: Expression,
    t :: Type
  } -> Expression

  ParenthesizedExpression :: {
    open :: Token,
    inner :: Expression,
    close :: Token,
    t :: Type
  } -> Expression

  deriving (Eq, Show, Read)


data UnaryOperator where
  PlusOperator :: {span :: (Int, Int), t :: Type} -> UnaryOperator
  MinusOperator :: {span :: (Int, Int), t :: Type} -> UnaryOperator
  NotOperator :: {span :: (Int, Int), t :: Type} -> UnaryOperator
  deriving (Eq, Show, Read)


data BinaryOperator where
  AddOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  SubtractOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  MultiplyOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  DivideOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  RemainderOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  EqualOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  NotEqualOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  LessOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  LessOrEqualOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  GreaterOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  GreaterOrEqualOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  AndOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  OrOperator :: {span :: (Int, Int), t :: Type} -> BinaryOperator
  deriving (Eq, Show, Read)


data AssignOperator where
  AssignOperator :: {span :: (Int, Int), t :: Type} -> AssignOperator
  AddAssignOperator :: {span :: (Int, Int), t :: Type} -> AssignOperator
  SubtractAssignOperator :: {span :: (Int, Int), t :: Type} -> AssignOperator
  MultiplyAssignOperator :: {span :: (Int, Int), t :: Type} -> AssignOperator
  DivideAssignOperator :: {span :: (Int, Int), t :: Type} -> AssignOperator
  RemainderAssignOperator :: {span :: (Int, Int), t :: Type} -> AssignOperator
  deriving (Eq, Show, Read)


data Identifier = Identifier {span :: (Int, Int), name :: Text, t :: Type}
  deriving (Eq, Show, Read)


newtype Token = Token {span :: (Int, Int)}
  deriving (Eq, Show, Read)


newtype Comment = Comment {span :: (Int, Int)}
  deriving (Eq, Show, Read)


data CallTarget where
  Undefined :: CallTarget
  IntToInt :: CallTarget
  FloatToInt :: CallTarget
  IntToFloat :: CallTarget
  FloatToFloat :: CallTarget
  UserDefined :: {parameters :: [Identifier], body :: Statement} -> CallTarget
  deriving (Eq, Show, Read)


instance Span Declaration where
  start VariableDeclaration{varKeyword} = Span.start varKeyword
  start FunctionDeclaration{defKeyword} = Span.start defKeyword

  end VariableDeclaration{semicolon} = Span.end semicolon
  end FunctionDeclaration{body} = Span.end body


instance Node Declaration where
  label VariableDeclaration{} = "VariableDeclaration"
  label FunctionDeclaration{} = "FunctionDeclaration"

  isLeaf _ = False


instance Span Statement where
  start ExpressionStatement{value} = Span.start value
  start IfStatement{ifKeyword} = Span.start ifKeyword
  start IfElseStatement{ifKeyword} = Span.start ifKeyword
  start WhileStatement{whileKeyword} = Span.start whileKeyword
  start DoWhileStatement{doKeyword} = Span.start doKeyword
  start ReturnStatement{returnKeyword} = Span.start returnKeyword
  start BlockStatement{open} = Span.start open

  end ExpressionStatement{semicolon} = Span.end semicolon
  end IfStatement{trueBranch} = Span.end trueBranch
  end IfElseStatement{falseBranch} = Span.end falseBranch
  end WhileStatement{body} = Span.end body
  end DoWhileStatement{body} = Span.end body
  end ReturnStatement{semicolon} = Span.end semicolon
  end BlockStatement{close} = Span.end close


instance Node Statement where
  label ExpressionStatement{} = "ExpressionStatement"
  label IfStatement{} = "IfStatement"
  label IfElseStatement{} = "IfElseStatement"
  label WhileStatement{} = "WhileStatement"
  label DoWhileStatement{} = "DoWhileStatement"
  label ReturnStatement{} = "ReturnStatement"
  label BlockStatement{} = "BlockStatement"

  isLeaf ReturnStatement{result = Nothing} = True
  isLeaf _ = False


instance Span Expression where
  start IntegerExpression{span} = Span.start span
  start RationalExpression{span} = Span.start span
  start VariableExpression{variableId} = Span.start variableId
  start CallExpression{targetId} = Span.start targetId
  start UnaryExpression{unary} = Span.start unary
  start BinaryExpression{left} = Span.start left
  start AssignExpression{targetId} = Span.start targetId
  start ParenthesizedExpression{open} = Span.start open

  end IntegerExpression{span} = Span.end span
  end RationalExpression{span} = Span.end span
  end VariableExpression{variableId} = Span.end variableId
  end CallExpression{close} = Span.end close
  end UnaryExpression{operand} = Span.end operand
  end BinaryExpression{right} = Span.end right
  end AssignExpression{value} = Span.end value
  end ParenthesizedExpression{close} = Span.end close


instance Node Expression where
  label IntegerExpression{} = "IntegerExpression"
  label RationalExpression{} = "RationalExpression"
  label VariableExpression{} = "VariableExpression"
  label CallExpression{} = "CallExpression"
  label UnaryExpression{} = "UnaryExpression"
  label BinaryExpression{} = "BinaryExpression"
  label AssignExpression{} = "AssignExpression"
  label ParenthesizedExpression{} = "ParenthesizedExpression"

  isLeaf IntegerExpression{} = True
  isLeaf RationalExpression{} = True
  isLeaf _ = False


instance Span UnaryOperator where
  start PlusOperator{span} = Span.start span
  start MinusOperator{span} = Span.start span
  start NotOperator{span} = Span.start span

  end PlusOperator{span} = Span.start span
  end MinusOperator{span} = Span.start span
  end NotOperator{span} = Span.start span


instance Node UnaryOperator where
  label PlusOperator{} = "PlusOperator"
  label MinusOperator{} = "MinusOperator"
  label NotOperator{} = "NotOperator"

  isLeaf _ = True


instance Span BinaryOperator where
  start AddOperator{span} = Span.start span
  start SubtractOperator{span} = Span.start span
  start MultiplyOperator{span} = Span.start span
  start DivideOperator{span} = Span.start span
  start RemainderOperator{span} = Span.start span
  start EqualOperator{span} = Span.start span
  start NotEqualOperator{span} = Span.start span
  start LessOperator{span} = Span.start span
  start LessOrEqualOperator{span} = Span.start span
  start GreaterOperator{span} = Span.start span
  start GreaterOrEqualOperator{span} = Span.start span
  start AndOperator{span} = Span.start span
  start OrOperator{span} = Span.start span

  end AddOperator{span} = Span.end span
  end SubtractOperator{span} = Span.end span
  end MultiplyOperator{span} = Span.end span
  end DivideOperator{span} = Span.end span
  end RemainderOperator{span} = Span.end span
  end EqualOperator{span} = Span.end span
  end NotEqualOperator{span} = Span.end span
  end LessOperator{span} = Span.end span
  end LessOrEqualOperator{span} = Span.end span
  end GreaterOperator{span} = Span.end span
  end GreaterOrEqualOperator{span} = Span.end span
  end AndOperator{span} = Span.end span
  end OrOperator{span} = Span.end span


instance Node BinaryOperator where
  label AddOperator{} = "AddOperator"
  label SubtractOperator{} = "SubtractOperator"
  label MultiplyOperator{} = "MultiplyOperator"
  label DivideOperator{} = "DivideOperator"
  label RemainderOperator{} = "RemainderOperator"
  label EqualOperator{} = "EqualOperator"
  label NotEqualOperator{} = "NotEqualOperator"
  label LessOperator{} = "LessOperator"
  label LessOrEqualOperator{} = "LessOrEqualOperator"
  label GreaterOperator{} = "GreaterOperator"
  label GreaterOrEqualOperator{} = "GreaterOrEqualOperator"
  label AndOperator{} = "AndOperator"
  label OrOperator{} = "OrOperator"

  isLeaf _ = True


instance Span AssignOperator where
  start AssignOperator{span} = Span.start span
  start AddAssignOperator{span} = Span.start span
  start SubtractAssignOperator{span} = Span.start span
  start MultiplyAssignOperator{span} = Span.start span
  start DivideAssignOperator{span} = Span.start span
  start RemainderAssignOperator{span} = Span.start span

  end AssignOperator{span} = Span.end span
  end AddAssignOperator{span} = Span.end span
  end SubtractAssignOperator{span} = Span.end span
  end MultiplyAssignOperator{span} = Span.end span
  end DivideAssignOperator{span} = Span.end span
  end RemainderAssignOperator{span} = Span.end span


instance Node AssignOperator where
  label AssignOperator{} = "AssignOperator"
  label AddAssignOperator{} = "AddAssignOperator"
  label SubtractAssignOperator{} = "SubtractAssignOperator"
  label MultiplyAssignOperator{} = "MultiplyAssignOperator"
  label DivideAssignOperator{} = "DivideAssignOperator"
  label RemainderAssignOperator{} = "RemainderAssignOperator"

  isLeaf _ = True


instance Span Identifier where
  start Identifier{span} = Span.start span

  end Identifier{span} = Span.end span


instance Node Identifier where
  label Identifier{} = "Identifier"

  isLeaf Identifier{} = True


instance Span Token where
  start Token{span} = Span.start span

  end Token{span} = Span.end span


instance Node Token where
  label Token{} = "Token"

  isLeaf Token{} = True


instance Span Comment where
  start Comment{span} = Span.start span

  end Comment{span} = Span.end span


instance Node Comment where
  label Comment{} = "Comment"

  isLeaf Comment{} = True


doesReturn :: Statement -> Bool
doesReturn ExpressionStatement{} = False
doesReturn IfStatement{} = False
doesReturn IfElseStatement{trueBranch, falseBranch} = doesReturn trueBranch && doesReturn falseBranch
doesReturn WhileStatement{} = False
doesReturn DoWhileStatement{body} = doesReturn body
doesReturn ReturnStatement{} = True
doesReturn BlockStatement{elements} = any (either (const False) doesReturn) elements


hasSideEffects :: Expression -> Bool
hasSideEffects IntegerExpression{} = False
hasSideEffects RationalExpression{} = False
hasSideEffects VariableExpression{} = False
hasSideEffects CallExpression{} = True
hasSideEffects UnaryExpression{operand} = hasSideEffects operand
hasSideEffects BinaryExpression{left, right} = hasSideEffects left || hasSideEffects right
hasSideEffects AssignExpression{} = True
hasSideEffects ParenthesizedExpression{inner} = hasSideEffects inner


comparePrecedence :: BinaryOperator -> BinaryOperator -> Ordering
comparePrecedence = comparing precedence
  where
    precedence AddOperator{} = 4
    precedence SubtractOperator{} = 4
    precedence MultiplyOperator{} = 5
    precedence DivideOperator{} = 5
    precedence RemainderOperator{} = 5
    precedence EqualOperator{} = 2
    precedence NotEqualOperator{} = 2
    precedence LessOperator{} = 3
    precedence LessOrEqualOperator{} = 3
    precedence GreaterOperator{} = 3
    precedence GreaterOrEqualOperator{} = 3
    precedence AndOperator{} = 1
    precedence OrOperator{} = 1
