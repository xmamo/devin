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
  comparePrecedence
) where

import Prelude hiding (span)
import Data.Ord

import Data.Text (Text)

import Span (Span (Span))
import qualified Span


class Syntax a where
  {-# MINIMAL span | start, end #-}

  span :: a -> Span
  span syntax = Span (start syntax) (end syntax)

  start :: Integral b => a -> b
  start = Span.start . span

  end :: Integral b => a -> b
  end = Span.end . span


data Declaration a where
  VariableDeclaration :: {
    varKeyword :: Token,
    variableId :: Identifier (),
    typeInfo :: Maybe (Token, Identifier ()),
    equalSign :: Token,
    value :: Expression a,
    semicolon :: Token
  } -> Declaration a

  FunctionDeclaration :: {
    defKeyword :: Token,
    functionId :: Identifier (),
    open :: Token,
    parameters :: Maybe ((Identifier (), Token, Identifier ()), [(Token, Identifier (), Token, Identifier ())]),
    close :: Token,
    returnInfo :: Maybe (Token, Identifier ()),
    body :: Statement a
  } -> Declaration a

  deriving (Eq, Show, Read)


data Statement a where
  ExpressionStatement :: {
    value :: Expression a,
    semicolon :: Token
  } -> Statement a

  IfStatement :: {
    ifKeyword :: Token,
    predicate :: Expression a,
    trueBranch :: Statement a
  } -> Statement a

  IfElseStatement :: {
    ifKeyword :: Token,
    predicate :: Expression a,
    trueBranch :: Statement a,
    elseKeyword :: Token,
    falseBranch :: Statement a
  } -> Statement a

  WhileStatement :: {
    whileKeyword :: Token,
    predicate :: Expression a,
    body :: Statement a
  } -> Statement a

  DoWhileStatement :: {
    doKeyword :: Token,
    body :: Statement a,
    whileKeyword :: Token,
    predicate :: Expression a,
    semicolon :: Token
  } -> Statement a

  ReturnStatement :: {
    returnKeyword :: Token,
    result :: Maybe (Expression a),
    semicolon :: Token
  } -> Statement a

  BlockStatement :: {
    open :: Token,
    elements :: [Either (Declaration a) (Statement a)],
    close :: Token
  } -> Statement a

  deriving (Eq, Show, Read)

data Expression a where
  IntegerExpression :: {
    source :: Span,
    integer :: Integer,
    extra :: a
  } -> Expression a

  RationalExpression :: {
    source :: Span,
    rational :: Rational,
    extra :: a
  } -> Expression a

  VariableExpression :: {
    variableId :: Identifier a,
    extra :: a
  } -> Expression a

  CallExpression :: {
    targetId :: Identifier a,
    open :: Token,
    arguments :: Maybe (Expression a, [(Token, Expression a)]),
    close :: Token,
    extra :: a
  } -> Expression a

  UnaryExpression :: {
    unary :: UnaryOperator a,
    operand :: Expression a,
    extra :: a
  } -> Expression a

  BinaryExpression :: {
    left :: Expression a,
    binary :: BinaryOperator a,
    right :: Expression a,
    extra :: a
  } -> Expression a

  AssignExpression :: {
    targetId :: Identifier a,
    assign :: AssignOperator a,
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


data UnaryOperator a where
  PlusOperator :: {source :: Span, extra :: a} -> UnaryOperator a
  MinusOperator :: {source :: Span, extra :: a} -> UnaryOperator a
  NotOperator :: {source :: Span, extra :: a} -> UnaryOperator a
  deriving (Eq, Show, Read)


data BinaryOperator a where
  AddOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  SubtractOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  MultiplyOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  DivideOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  RemainderOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  EqualOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  NotEqualOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  LessOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  LessOrEqualOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  GreaterOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  GreaterOrEqualOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  AndOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  OrOperator :: {source :: Span, extra :: a} -> BinaryOperator a
  deriving (Eq, Show, Read)


data AssignOperator a where
  AssignOperator :: {source :: Span, extra :: a} -> AssignOperator a
  AddAssignOperator :: {source :: Span, extra :: a} -> AssignOperator a
  SubtractAssignOperator :: {source :: Span, extra :: a} -> AssignOperator a
  MultiplyAssignOperator :: {source :: Span, extra :: a} -> AssignOperator a
  DivideAssignOperator :: {source :: Span, extra :: a} -> AssignOperator a
  RemainderAssignOperator :: {source :: Span, extra :: a} -> AssignOperator a
  deriving (Eq, Show, Read)


data Identifier a where
  Identifier :: {source :: Span, name :: Text, extra :: a} -> Identifier a
  deriving (Eq, Show, Read)


data Token where
  Token :: {source :: Span} -> Token
  deriving (Eq, Show, Read)


data Comment where
  Comment :: {source :: Span} -> Comment
  deriving (Eq, Show, Read)


instance Syntax (Declaration a) where
  start VariableDeclaration {varKeyword} = start varKeyword
  start FunctionDeclaration {defKeyword} = start defKeyword

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
  span IntegerExpression {source} = source
  span RationalExpression {source} = source
  span VariableExpression {variableId} = span variableId
  span expression = Span (start expression) (end expression)

  start CallExpression {targetId} = start targetId
  start UnaryExpression {unary} = start unary
  start BinaryExpression {left} = start left
  start AssignExpression {targetId} = start targetId
  start ParenthesizedExpression {open} = start open
  start expression = Span.start (span expression)

  end CallExpression {close} = end close
  end UnaryExpression {operand} = end operand
  end BinaryExpression {right} = end right
  end AssignExpression {value} = end value
  end ParenthesizedExpression {close} = end close
  end expression = Span.end (span expression)


instance Syntax (UnaryOperator a) where
  span PlusOperator {source} = source
  span MinusOperator {source} = source
  span NotOperator {source} = source


instance Syntax (AssignOperator a) where
  span AssignOperator {source} = source
  span AddAssignOperator {source} = source
  span SubtractAssignOperator {source} = source
  span MultiplyAssignOperator {source} = source
  span DivideAssignOperator {source} = source
  span RemainderAssignOperator {source} = source


instance Syntax (BinaryOperator a) where
  span AddOperator {source} = source
  span SubtractOperator {source} = source
  span MultiplyOperator {source} = source
  span DivideOperator {source} = source
  span RemainderOperator {source} = source
  span EqualOperator {source} = source
  span NotEqualOperator {source} = source
  span LessOperator {source} = source
  span LessOrEqualOperator {source} = source
  span GreaterOperator {source} = source
  span GreaterOrEqualOperator {source} = source
  span AndOperator {source} = source
  span OrOperator {source} = source


instance Syntax (Identifier a) where
  span Identifier {source} = source


instance Syntax Token where
  span Token {source} = source


instance Syntax Comment where
  span Comment {source} = source


comparePrecedence :: BinaryOperator a -> BinaryOperator a -> Ordering
comparePrecedence = comparing precedence
  where
    precedence AddOperator {} = 4
    precedence SubtractOperator {} = 4
    precedence MultiplyOperator {} = 5
    precedence DivideOperator {} = 5
    precedence RemainderOperator {} = 5
    precedence EqualOperator {} = 2
    precedence NotEqualOperator {} = 2
    precedence LessOperator {} = 3
    precedence LessOrEqualOperator {} = 3
    precedence GreaterOperator {} = 3
    precedence GreaterOrEqualOperator {} = 3
    precedence AndOperator {} = 1
    precedence OrOperator {} = 1
