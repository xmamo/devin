module Syntax (
  Syntax (..),
  Declaration (..),
  Statement (..),
  Expression (..),
  UnaryOperator (..),
  BinaryOperator (..),
  AssignOperator (..),
  Identifier (..),
  Token (..),
  Comment (..),
  doesReturn,
  hasSideEffects,
  comparePrecedence
) where

import Prelude hiding (span)
import Data.Ord

import Data.Text (Text)

import {-# SOURCE #-} CallTarget (CallTarget)
import Span (Span (Span))
import qualified Span
import Type (Type)


class Syntax a where
  {-# MINIMAL span | start, end #-}

  span :: a -> Span
  span syntax = Span (start syntax) (end syntax)

  start :: Num b => a -> b
  start = Span.start . span

  end :: Num b => a -> b
  end = Span.end . span


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
    s :: Span,
    integer :: Integer,
    t :: Type
  } -> Expression

  RationalExpression :: {
    s :: Span,
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
  PlusOperator :: {s :: Span, t :: Type} -> UnaryOperator
  MinusOperator :: {s :: Span, t :: Type} -> UnaryOperator
  NotOperator :: {s :: Span, t :: Type} -> UnaryOperator
  deriving (Eq, Show, Read)


data BinaryOperator where
  AddOperator :: {s :: Span, t :: Type} -> BinaryOperator
  SubtractOperator :: {s :: Span, t :: Type} -> BinaryOperator
  MultiplyOperator :: {s :: Span, t :: Type} -> BinaryOperator
  DivideOperator :: {s :: Span, t :: Type} -> BinaryOperator
  RemainderOperator :: {s :: Span, t :: Type} -> BinaryOperator
  EqualOperator :: {s :: Span, t :: Type} -> BinaryOperator
  NotEqualOperator :: {s :: Span, t :: Type} -> BinaryOperator
  LessOperator :: {s :: Span, t :: Type} -> BinaryOperator
  LessOrEqualOperator :: {s :: Span, t :: Type} -> BinaryOperator
  GreaterOperator :: {s :: Span, t :: Type} -> BinaryOperator
  GreaterOrEqualOperator :: {s :: Span, t :: Type} -> BinaryOperator
  AndOperator :: {s :: Span, t :: Type} -> BinaryOperator
  OrOperator :: {s :: Span, t :: Type} -> BinaryOperator
  deriving (Eq, Show, Read)


data AssignOperator where
  AssignOperator :: {s :: Span, t :: Type} -> AssignOperator
  AddAssignOperator :: {s :: Span, t :: Type} -> AssignOperator
  SubtractAssignOperator :: {s :: Span, t :: Type} -> AssignOperator
  MultiplyAssignOperator :: {s :: Span, t :: Type} -> AssignOperator
  DivideAssignOperator :: {s :: Span, t :: Type} -> AssignOperator
  RemainderAssignOperator :: {s :: Span, t :: Type} -> AssignOperator
  deriving (Eq, Show, Read)


data Identifier where
  Identifier :: {s :: Span, name :: Text, t :: Type} -> Identifier
  deriving (Eq, Show, Read)


data Token where
  Token :: {s :: Span} -> Token
  deriving (Eq, Show, Read)


data Comment where
  Comment :: {s :: Span} -> Comment
  deriving (Eq, Show, Read)


instance Syntax Declaration where
  start VariableDeclaration{varKeyword} = start varKeyword
  start FunctionDeclaration{defKeyword} = start defKeyword

  end VariableDeclaration{semicolon} = end semicolon
  end FunctionDeclaration{body} = end body


instance Syntax Statement where
  start ExpressionStatement{value} = start value
  start IfStatement{ifKeyword} = start ifKeyword
  start IfElseStatement{ifKeyword} = start ifKeyword
  start WhileStatement{whileKeyword} = start whileKeyword
  start DoWhileStatement{doKeyword} = start doKeyword
  start ReturnStatement{returnKeyword} = start returnKeyword
  start BlockStatement{open} = start open

  end ExpressionStatement{semicolon} = end semicolon
  end IfStatement{trueBranch} = end trueBranch
  end IfElseStatement{falseBranch} = end falseBranch
  end WhileStatement{body} = end body
  end DoWhileStatement{body} = end body
  end ReturnStatement{semicolon} = end semicolon
  end BlockStatement{close} = end close


instance Syntax Expression where
  span IntegerExpression{s} = s
  span RationalExpression{s} = s
  span VariableExpression{variableId} = span variableId
  span expression = Span (start expression) (end expression)

  start CallExpression{targetId} = start targetId
  start UnaryExpression{unary} = start unary
  start BinaryExpression{left} = start left
  start AssignExpression{targetId} = start targetId
  start ParenthesizedExpression{open} = start open
  start expression = Span.start (span expression)

  end CallExpression{close} = end close
  end UnaryExpression{operand} = end operand
  end BinaryExpression{right} = end right
  end AssignExpression{value} = end value
  end ParenthesizedExpression{close} = end close
  end expression = Span.end (span expression)


instance Syntax UnaryOperator where
  span PlusOperator{s} = s
  span MinusOperator{s} = s
  span NotOperator{s} = s


instance Syntax BinaryOperator where
  span AddOperator{s} = s
  span SubtractOperator{s} = s
  span MultiplyOperator{s} = s
  span DivideOperator{s} = s
  span RemainderOperator{s} = s
  span EqualOperator{s} = s
  span NotEqualOperator{s} = s
  span LessOperator{s} = s
  span LessOrEqualOperator{s} = s
  span GreaterOperator{s} = s
  span GreaterOrEqualOperator{s} = s
  span AndOperator{s} = s
  span OrOperator{s} = s


instance Syntax AssignOperator where
  span AssignOperator{s} = s
  span AddAssignOperator{s} = s
  span SubtractAssignOperator{s} = s
  span MultiplyAssignOperator{s} = s
  span DivideAssignOperator{s} = s
  span RemainderAssignOperator{s} = s


instance Syntax Identifier where
  span Identifier{s} = s


instance Syntax Token where
  span Token{s} = s


instance Syntax Comment where
  span Comment{s} = s


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
