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
import Data.Bifunctor
import Data.Ord

import Data.Text (Text)

import Span (Span (Span))
import qualified Span


class Syntax a where
  {-# MINIMAL span | start, end #-}

  span :: a -> Span
  span syntax = Span (start syntax) (end syntax)

  start :: Num b => a -> b
  start = Span.start . span

  end :: Num b => a -> b
  end = Span.end . span


data Declaration a where
  VariableDeclaration :: {
    varKeyword :: Token,
    variableId :: Identifier a,
    typeInfo :: Maybe (Token, Identifier a),
    equalSign :: Token,
    value :: Expression a,
    semicolon :: Token
  } -> Declaration a

  FunctionDeclaration :: {
    defKeyword :: Token,
    functionId :: Identifier a,
    open :: Token,
    parameters :: Maybe (Identifier a, Token, Identifier a, [(Token, Identifier a, Token, Identifier a)]),
    close :: Token,
    returnInfo :: Maybe (Token, Identifier a),
    body :: Statement a
  } -> Declaration a

  deriving (Eq, Functor, Show, Read)

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
    s :: Span,
    integer :: Integer,
    t :: a
  } -> Expression a

  RationalExpression :: {
    s :: Span,
    rational :: Rational,
    t :: a
  } -> Expression a

  VariableExpression :: {
    variableId :: Identifier a,
    t :: a
  } -> Expression a

  CallExpression :: {
    targetId :: Identifier a,
    open :: Token,
    arguments :: Maybe (Expression a, [(Token, Expression a)]),
    close :: Token,
    t :: a
  } -> Expression a

  UnaryExpression :: {
    unary :: UnaryOperator a,
    operand :: Expression a,
    t :: a
  } -> Expression a

  BinaryExpression :: {
    left :: Expression a,
    binary :: BinaryOperator a,
    right :: Expression a,
    t :: a
  } -> Expression a

  AssignExpression :: {
    targetId :: Identifier a,
    assign :: AssignOperator a,
    value :: Expression a,
    t :: a
  } -> Expression a

  ParenthesizedExpression :: {
    open :: Token,
    inner :: Expression a,
    close :: Token,
    t :: a
  } -> Expression a

  deriving (Eq, Functor, Show, Read)


data UnaryOperator a where
  PlusOperator :: {s :: Span, t :: a} -> UnaryOperator a
  MinusOperator :: {s :: Span, t :: a} -> UnaryOperator a
  NotOperator :: {s :: Span, t :: a} -> UnaryOperator a
  deriving (Eq, Functor, Show, Read)


data BinaryOperator a where
  AddOperator :: {s :: Span, t :: a} -> BinaryOperator a
  SubtractOperator :: {s :: Span, t :: a} -> BinaryOperator a
  MultiplyOperator :: {s :: Span, t :: a} -> BinaryOperator a
  DivideOperator :: {s :: Span, t :: a} -> BinaryOperator a
  RemainderOperator :: {s :: Span, t :: a} -> BinaryOperator a
  EqualOperator :: {s :: Span, t :: a} -> BinaryOperator a
  NotEqualOperator :: {s :: Span, t :: a} -> BinaryOperator a
  LessOperator :: {s :: Span, t :: a} -> BinaryOperator a
  LessOrEqualOperator :: {s :: Span, t :: a} -> BinaryOperator a
  GreaterOperator :: {s :: Span, t :: a} -> BinaryOperator a
  GreaterOrEqualOperator :: {s :: Span, t :: a} -> BinaryOperator a
  AndOperator :: {s :: Span, t :: a} -> BinaryOperator a
  OrOperator :: {s :: Span, t :: a} -> BinaryOperator a
  deriving (Eq, Functor, Show, Read)


data AssignOperator a where
  AssignOperator :: {s :: Span, t :: a} -> AssignOperator a
  AddAssignOperator :: {s :: Span, t :: a} -> AssignOperator a
  SubtractAssignOperator :: {s :: Span, t :: a} -> AssignOperator a
  MultiplyAssignOperator :: {s :: Span, t :: a} -> AssignOperator a
  DivideAssignOperator :: {s :: Span, t :: a} -> AssignOperator a
  RemainderAssignOperator :: {s :: Span, t :: a} -> AssignOperator a
  deriving (Eq, Functor, Show, Read)


data Identifier a where
  Identifier :: {s :: Span, name :: Text, t :: a} -> Identifier a
  deriving (Eq, Functor, Show, Read)


data Token where
  Token :: {s :: Span} -> Token
  deriving (Eq, Show, Read)


data Comment where
  Comment :: {s :: Span} -> Comment
  deriving (Eq, Show, Read)


instance Syntax (Declaration a) where
  start VariableDeclaration{varKeyword} = start varKeyword
  start FunctionDeclaration{defKeyword} = start defKeyword

  end VariableDeclaration{semicolon} = end semicolon
  end FunctionDeclaration{body} = end body


instance Functor Statement where
  fmap f = \case
    ExpressionStatement{value, semicolon} ->
      ExpressionStatement (f <$> value) semicolon

    IfStatement{ifKeyword, predicate, trueBranch} ->
      IfStatement ifKeyword (f <$> predicate) (f <$> trueBranch)

    IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} ->
      IfElseStatement ifKeyword (f <$> predicate) (f <$> trueBranch) elseKeyword (f <$> falseBranch)

    WhileStatement{whileKeyword, predicate, body} ->
      WhileStatement whileKeyword (f <$> predicate) (f <$> body)

    DoWhileStatement{doKeyword, body, whileKeyword, predicate, semicolon} ->
      DoWhileStatement doKeyword (f <$> body) whileKeyword (f <$> predicate) semicolon

    ReturnStatement{returnKeyword, result, semicolon} ->
      ReturnStatement returnKeyword (fmap f <$> result) semicolon

    BlockStatement{open, elements, close} ->
      BlockStatement open (bimap (fmap f) (fmap f) <$> elements) close


instance Syntax (Statement a) where
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


instance Syntax (Expression a) where
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


instance Syntax (UnaryOperator a) where
  span PlusOperator{s} = s
  span MinusOperator{s} = s
  span NotOperator{s} = s


instance Syntax (BinaryOperator a) where
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


instance Syntax (AssignOperator a) where
  span AssignOperator{s} = s
  span AddAssignOperator{s} = s
  span SubtractAssignOperator{s} = s
  span MultiplyAssignOperator{s} = s
  span DivideAssignOperator{s} = s
  span RemainderAssignOperator{s} = s


instance Syntax (Identifier a) where
  span Identifier{s} = s


instance Syntax Token where
  span Token{s} = s


instance Syntax Comment where
  span Comment{s} = s


doesReturn :: Statement a -> Bool
doesReturn ExpressionStatement{} = False
doesReturn IfStatement{} = False
doesReturn IfElseStatement{trueBranch, falseBranch} = doesReturn trueBranch && doesReturn falseBranch
doesReturn WhileStatement{} = False
doesReturn DoWhileStatement{body} = doesReturn body
doesReturn ReturnStatement{} = True
doesReturn BlockStatement{elements} = any (either (const False) doesReturn) elements


hasSideEffects :: Expression a -> Bool
hasSideEffects IntegerExpression{} = False
hasSideEffects RationalExpression{} = False
hasSideEffects VariableExpression{} = False
hasSideEffects CallExpression{} = True
hasSideEffects UnaryExpression{operand} = hasSideEffects operand
hasSideEffects BinaryExpression{left, right} = hasSideEffects left || hasSideEffects right
hasSideEffects AssignExpression{} = True
hasSideEffects ParenthesizedExpression{inner} = hasSideEffects inner


comparePrecedence :: BinaryOperator a -> BinaryOperator a -> Ordering
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
