module Syntax (
  Syntax (..),
  Declaration (..),
  Statement (..),
  Expression (..),
  UnaryOperator (..),
  BinaryOperator (..),
  AssignOperator (..),
  Integer (..),
  Identifier (..),
  Token (..),
  Comment (..),
  comparePrecedence
) where

import Data.Ord
import Prelude hiding (Integer, span)
import qualified Prelude

import Data.Text (Text)

import Span (Span (Span))
import qualified Span


class Syntax a where
  span :: a -> Span
  span syntax = Span (start syntax) (end syntax)

  start :: Integral b => a -> b
  start = Span.start . span

  end :: Integral b => a -> b
  end = Span.end . span

  {-# MINIMAL span | start, end #-}


data Declaration a where
  EmptyVariableDeclaration :: {
    varKeyword :: Token,
    variable :: Identifier,
    colon :: Token,
    tName :: Identifier,
    semicolon :: Token,
    extra :: a
  } -> Declaration a

  VariableDeclaration :: {
    varKeyword :: Token,
    variable :: Identifier,
    colon :: Token,
    tName :: Identifier,
    equalSign :: Token,
    value :: Expression a,
    semicolon :: Token,
    extra :: a
  } -> Declaration a

  FunctionDeclaration :: {
    defKeyword :: Token,
    name :: Identifier,
    open :: Token,
    parameters :: Maybe ((Identifier, Token, Identifier), [(Token, Identifier, Token, Identifier)]),
    close :: Token,
    arrow :: Token,
    tName :: Identifier,
    body :: Statement a,
    extra :: a
  } -> Declaration a

  deriving (Eq, Show, Read)


data Statement a where
  ExpressionStatement :: {
    value :: Expression a,
    semicolon :: Token,
    extra :: a
  } -> Statement a

  IfStatement :: {
    ifKeyword :: Token,
    predicate :: Expression a,
    trueBranch :: Statement a,
    extra :: a
  } -> Statement a

  IfElseStatement :: {
    ifKeyword :: Token,
    predicate :: Expression a,
    trueBranch :: Statement a,
    elseKeyword :: Token,
    falseBranch :: Statement a,
    extra :: a
  } -> Statement a

  WhileStatement :: {
    whileKeyword :: Token,
    predicate :: Expression a,
    body :: Statement a,
    extra :: a
  } -> Statement a

  DoWhileStatement :: {
    doKeyword :: Token,
    body :: Statement a,
    whileKeyword :: Token,
    predicate :: Expression a,
    semicolon :: Token,
    extra :: a
  } -> Statement a

  ReturnStatement :: {
    returnKeyword :: Token,
    result :: Maybe (Expression a),
    semicolon :: Token,
    extra :: a
  } -> Statement a

  BlockStatement :: {
    open :: Token,
    elements :: [Either (Declaration a) (Statement a)],
    close :: Token,
    extra :: a
  } -> Statement a

  deriving (Eq, Show, Read)

data Expression a where
  IntegerExpression :: {
    integer :: Integer,
    extra :: a
  } -> Expression a

  VariableExpression :: {
    variable :: Identifier,
    extra :: a
  } -> Expression a

  CallExpression :: {
    target :: Identifier,
    open :: Token,
    arguments :: Maybe (Expression a, [(Token, Expression a)]),
    close :: Token,
    extra :: a
  } -> Expression a

  UnaryExpression :: {
    unary :: UnaryOperator,
    operand :: Expression a,
    extra :: a
  } -> Expression a

  BinaryExpression :: {
    left :: Expression a,
    binary :: BinaryOperator,
    right :: Expression a,
    extra :: a
  } -> Expression a

  AssignExpression :: {
    target :: Identifier,
    assign :: AssignOperator,
    value :: Expression a,
    extra :: a
  } -> Expression a

  ParenthesizedExpression :: {
    open :: Token,
    inner :: Expression a,
    close :: Token,
    extra :: a
  } -> Expression a

  deriving (Eq, Show, Read)


data UnaryOperator where
  PlusOperator :: Span -> UnaryOperator
  MinusOperator :: Span -> UnaryOperator
  NotOperator :: Span -> UnaryOperator
  deriving (Eq, Show, Read)


data BinaryOperator where
  AddOperator :: Span -> BinaryOperator
  SubtractOperator :: Span -> BinaryOperator
  MultiplyOperator :: Span -> BinaryOperator
  DivideOperator :: Span -> BinaryOperator
  RemainderOperator :: Span -> BinaryOperator
  EqualOperator :: Span -> BinaryOperator
  NotEqualOperator :: Span -> BinaryOperator
  LessOperator :: Span -> BinaryOperator
  LessOrEqualOperator :: Span -> BinaryOperator
  GreaterOperator :: Span -> BinaryOperator
  GreaterOrEqualOperator :: Span -> BinaryOperator
  AndOperator :: Span -> BinaryOperator
  OrOperator :: Span -> BinaryOperator
  deriving (Eq, Show, Read)


data AssignOperator where
  AssignOperator :: Span -> AssignOperator
  AddAssignOperator :: Span -> AssignOperator
  SubtractAssignOperator :: Span -> AssignOperator
  MultiplyAssignOperator :: Span -> AssignOperator
  DivideAssignOperator :: Span -> AssignOperator
  RemainderAssignOperator :: Span -> AssignOperator
  deriving (Eq, Show, Read)


data Integer where
  Integer :: Span -> Prelude.Integer -> Integer
  deriving (Eq, Show, Read)


data Identifier where
  Identifier :: Span -> Text -> Identifier
  deriving (Eq, Show, Read)


data Token where
  Token :: Span -> Token
  deriving (Eq, Show, Read)


data Comment where
  Comment :: Span -> Comment
  deriving (Eq, Show, Read)


instance Syntax (Declaration a) where
  start EmptyVariableDeclaration {varKeyword} = start varKeyword
  start VariableDeclaration {varKeyword} = start varKeyword
  start FunctionDeclaration {defKeyword} = start defKeyword

  end EmptyVariableDeclaration {semicolon} = end semicolon
  end VariableDeclaration {semicolon} = end semicolon
  end FunctionDeclaration {body} = end body


instance Syntax (Statement a) where
  start ExpressionStatement {value} = start value
  start IfStatement {ifKeyword} = start ifKeyword
  start IfElseStatement {ifKeyword} = start ifKeyword
  start WhileStatement {whileKeyword} = start whileKeyword
  start DoWhileStatement {doKeyword} = start doKeyword
  start ReturnStatement {returnKeyword} = start returnKeyword
  start BlockStatement {open} = start open

  end ExpressionStatement {semicolon} = end semicolon
  end IfStatement {trueBranch} = end trueBranch
  end IfElseStatement {falseBranch} = end falseBranch
  end WhileStatement {body} = end body
  end DoWhileStatement {body} = end body
  end ReturnStatement {semicolon} = end semicolon
  end BlockStatement {close} = end close


instance Syntax (Expression a) where
  span IntegerExpression {integer} = span integer
  span VariableExpression {variable} = span variable
  span expression = Span (start expression) (end expression)

  start CallExpression {target} = start target
  start UnaryExpression {unary} = start unary
  start BinaryExpression {left} = start left
  start AssignExpression {target} = start target
  start ParenthesizedExpression {open} = start open
  start expression = Span.start (span expression)

  end CallExpression {close} = end close
  end UnaryExpression {operand} = end operand
  end BinaryExpression {right} = end right
  end AssignExpression {value} = end value
  end ParenthesizedExpression {close} = end close
  end expression = Span.end (span expression)


instance Syntax UnaryOperator where
  span (PlusOperator s) = s
  span (MinusOperator s) = s
  span (NotOperator s) = s


instance Syntax AssignOperator where
  span (AssignOperator s) = s
  span (AddAssignOperator s) = s
  span (SubtractAssignOperator s) = s
  span (MultiplyAssignOperator s) = s
  span (DivideAssignOperator s) = s
  span (RemainderAssignOperator s) = s


instance Syntax BinaryOperator where
  span (AddOperator s) = s
  span (SubtractOperator s) = s
  span (MultiplyOperator s) = s
  span (DivideOperator s) = s
  span (RemainderOperator s) = s
  span (EqualOperator s) = s
  span (NotEqualOperator s) = s
  span (LessOperator s) = s
  span (LessOrEqualOperator s) = s
  span (GreaterOperator s) = s
  span (GreaterOrEqualOperator s) = s
  span (AndOperator s) = s
  span (OrOperator s) = s


instance Syntax Integer where
  span (Integer s _) = s


instance Syntax Identifier where
  span (Identifier s _) = s


instance Syntax Token where
  span (Token s) = s


instance Syntax Comment where
  span (Comment s) = s


comparePrecedence :: BinaryOperator -> BinaryOperator -> Ordering
comparePrecedence = comparing precedence
  where
    precedence (AddOperator _) = 4
    precedence (SubtractOperator _) = 4
    precedence (MultiplyOperator _) = 5
    precedence (DivideOperator _) = 5
    precedence (RemainderOperator _) = 5
    precedence (EqualOperator _) = 2
    precedence (NotEqualOperator _) = 2
    precedence (LessOperator _) = 3
    precedence (LessOrEqualOperator _) = 3
    precedence (GreaterOperator _) = 3
    precedence (GreaterOrEqualOperator _) = 3
    precedence (AndOperator _) = 1
    precedence (OrOperator _) = 1
