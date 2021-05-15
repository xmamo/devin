module Syntax (
  Syntax (..),
  Token (..),
  Identifier(..),
  Expression (..),
  UnaryOperator (..),
  BinaryOperator (..),
  AssignOperator (..),
  precedence
) where

import Prelude hiding (span)

import Data.Text (Text)

import Span (Span)
import qualified Span


class Syntax a where
  span :: a -> Span

  start :: a -> Int
  start = Span.start . span

  end :: a -> Int
  end = Span.end . span


data Token where
  Token :: Span -> Token
  deriving (Eq, Show, Read)


data Identifier where
  Identifier :: Text -> Span -> Identifier
  deriving (Eq, Show, Read)


data Expression where
  IntegerExpression :: Integer -> Span -> Expression
  IdentifierExpression :: Identifier -> Span -> Expression
  UnaryExpression :: UnaryOperator -> Expression -> Span -> Expression
  BinaryExpression :: Expression -> BinaryOperator -> Expression -> Span -> Expression
  AssignExpression :: Identifier -> AssignOperator -> Expression -> Span -> Expression
  ParenthesizedExpression :: Token -> Expression -> Token -> Span -> Expression
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


instance Syntax Token where
  span (Token s) = s


instance Syntax Identifier where
  span (Identifier _ s) = s


instance Syntax Expression where
  span (IntegerExpression _ s) = s
  span (IdentifierExpression _ s) = s
  span (UnaryExpression _ _ s) = s
  span (BinaryExpression _ _ _ s) = s
  span (AssignExpression _ _ _ s) = s
  span (ParenthesizedExpression _ _ _ s) = s


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
