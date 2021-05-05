module Syntax (
  Syntax (..),

  Expression (
    IntegerExpression,
    IdentifierExpression,
    UnaryExpression,
    BinaryExpression,
    ParenthesizedExpression
  ),

  UnaryOperator (
    PlusOperator,
    MinusOperator,
    NotOperator
  ),

  BinaryOperator (
    AddOperator,
    SubtractOperator,
    MultiplyOperator,
    DivideOperator,
    RemainderOperator,
    EqualOperator,
    NotEqualOperator,
    LessOperator,
    LessOrEqualOperator,
    GreaterOperator,
    GreaterOrEqualOperator,
    AndOperator,
    OrOperator
  ),

  precedence
) where

import Prelude hiding (span)

import Data.Text (Text)

import Span (Span)


class Syntax a where
  span :: a -> Span


instance Syntax Expression where
  span = span'


instance Syntax UnaryOperator where
  span = span''


instance Syntax BinaryOperator where
  span = span'''


data Expression where
  IntegerExpression :: {
    integer :: Integer,
    span' :: Span
  } -> Expression

  IdentifierExpression :: {
    identifier :: Text,
    span' :: Span
  } -> Expression

  UnaryExpression :: {
    operator :: UnaryOperator,
    operand :: Expression,
    span' :: Span
  } -> Expression

  BinaryExpression :: {
    left:: Expression,
    operator' :: BinaryOperator,
    right :: Expression,
    span' :: Span
  } -> Expression

  ParenthesizedExpression :: {
    expression :: Expression,
    span' :: Span
  } -> Expression

  deriving (Eq, Show, Read)


data UnaryOperator where
  PlusOperator :: {span'' :: Span} -> UnaryOperator
  MinusOperator :: {span'' :: Span} -> UnaryOperator
  NotOperator :: {span'' :: Span} -> UnaryOperator
  deriving (Eq, Show, Read)


data BinaryOperator where
  AddOperator :: {span''' :: Span} -> BinaryOperator
  SubtractOperator :: {span''' :: Span} -> BinaryOperator
  MultiplyOperator :: {span''' :: Span} -> BinaryOperator
  DivideOperator :: {span''' :: Span} -> BinaryOperator
  RemainderOperator :: {span''' :: Span} -> BinaryOperator
  EqualOperator :: {span''' :: Span} -> BinaryOperator
  NotEqualOperator :: {span''' :: Span} -> BinaryOperator
  LessOperator :: {span''' :: Span} -> BinaryOperator
  LessOrEqualOperator :: {span''' :: Span} -> BinaryOperator
  GreaterOperator :: {span''' :: Span} -> BinaryOperator
  GreaterOrEqualOperator :: {span''' :: Span} -> BinaryOperator
  AndOperator :: {span''' :: Span} -> BinaryOperator
  OrOperator :: {span''' :: Span} -> BinaryOperator
  deriving (Eq, Show, Read)


precedence :: BinaryOperator -> Integer
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
