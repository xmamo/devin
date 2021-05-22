module Syntax (
  Syntax (..),
  Token (..),
  Identifier(..),
  Statement (..),
  Expression (..),
  UnaryOperator (..),
  BinaryOperator (..),
  AssignOperator (..),
  comparePrecedence
) where

import Prelude hiding (span)
import Data.Ord

import Data.Text (Text)

import Span (Span (Span))
import qualified Span


class Syntax a where
  span :: a -> Span
  span syntax = Span (start syntax) (end syntax)

  start :: Num b => a -> b
  start = Span.start . span

  end :: Num b => a -> b
  end = Span.end . span

  {-# MINIMAL span | start, end #-}


data Token where
  Token :: Span -> Token
  deriving (Eq, Show, Read)


data Identifier where
  Identifier :: Text -> Span -> Identifier
  deriving (Eq, Show, Read)


data Statement where
  DeclareStatement :: Identifier -> Identifier -> Token -> Statement
  DeclareAndAssignStatement :: Identifier -> Identifier -> Token -> Expression -> Token -> Statement
  ExpressionStatement :: Expression -> Token -> Statement
  IfStatement :: Token -> Expression -> Statement -> Statement
  IfElseStatement :: Token -> Expression -> Statement -> Token -> Statement -> Statement
  WhileStatement :: Token -> Expression -> Statement-> Statement
  DoWhileStatement :: Token -> Statement -> Token -> Expression -> Token -> Statement
  ReturnStatement :: Token -> Expression -> Token -> Statement
  BlockStatement :: Token -> [Statement] -> Token -> Statement
  deriving (Eq, Show, Read)


data Expression where
  IntegerExpression :: Integer -> Span -> Expression
  IdentifierExpression :: Identifier -> Expression
  UnaryExpression :: UnaryOperator -> Expression -> Expression
  BinaryExpression :: Expression -> BinaryOperator -> Expression -> Expression
  AssignExpression :: Identifier -> AssignOperator -> Expression -> Expression
  ParenthesizedExpression :: Token -> Expression -> Token -> Expression
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


instance Syntax Statement where
  start (DeclareStatement t _ _) = start t
  start (DeclareAndAssignStatement t _ _ _ _) = start t
  start (ExpressionStatement value _) = start value
  start (IfStatement ifKeyword _ _) = start ifKeyword
  start (IfElseStatement ifKeyword _ _ _ _) = start ifKeyword
  start (WhileStatement whileKeyword _ _) = start whileKeyword
  start (DoWhileStatement doKeyword _ _ _ _) = start doKeyword
  start (ReturnStatement returnKeyword _ _) = start returnKeyword
  start (BlockStatement open _ _) = start open

  end (DeclareStatement _ _ terminator) = end terminator
  end (DeclareAndAssignStatement _ _ _ _ terminator) = end terminator
  end (ExpressionStatement _ terminator) = end terminator
  end (IfStatement _ _ trueBranch) = end trueBranch
  end (IfElseStatement _ _ _ _ falseBranch) = end falseBranch
  end (WhileStatement _ _ body) = end body
  end (DoWhileStatement _ _ _ _ body) = end body
  end (ReturnStatement _ _ terminator) = end terminator
  end (BlockStatement _ _ close) = end close


instance Syntax Expression where
  span (IntegerExpression _ s) = s
  span (IdentifierExpression variable) = span variable
  span expression = Span (start expression) (end expression)

  start (UnaryExpression operator _) = start operator
  start (BinaryExpression left _ _) = start left
  start (AssignExpression target _ _) = start target
  start (ParenthesizedExpression open _ _) = start open
  start expression = Span.start (span expression)

  end (UnaryExpression _ operand) = end operand
  end (BinaryExpression _ _ right) = end right
  end (AssignExpression _ _ value) = end value
  end (ParenthesizedExpression _ _ close) = end close
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
