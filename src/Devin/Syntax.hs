{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Syntax (
  Devin (..),
  Definition (..),
  Statement (..),
  Expression (..),
  UnaryOperator (..),
  BinaryOperator (..),
  SymbolId (..),
  TypeId (..),
  Token (..)
) where

import Data.Data

import Devin.Display
import Devin.Interval
import Devin.Ratio


newtype Devin = Devin {definitions :: [Definition]}
  deriving (Eq, Show, Read, Data)


data Definition where
  VarDefinition :: {
    varKeyword :: Token,
    varId :: SymbolId,
    equalSign :: Token,
    value :: Expression,
    semicolon :: Token
  } -> Definition

  FunDefinition :: {
    defKeyword :: Token,
    funId :: SymbolId,
    open :: Token,
    params :: [(Maybe Token, SymbolId, Maybe (Token, TypeId))],
    commas :: [Token],
    close :: Token,
    returnInfo :: Maybe (Token, TypeId),
    body :: Statement
  } -> Definition

  deriving (Eq, Show, Read, Data)


data Statement where
  DefinitionStatement :: {
    definition :: Definition
  } -> Statement

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

  AssertStatement :: {
    assertKeyword :: Token,
    predicate :: Expression,
    semicolon :: Token
  } -> Statement

  BreakpointStatement :: {
    breakpointKeyword :: Token,
    semicolon :: Token
  } -> Statement

  BlockStatement :: {
    open :: Token,
    statements :: [Statement],
    close :: Token
  } -> Statement

  deriving (Eq, Show, Read, Data)


data Expression where
  VarExpression :: {
    varName :: String,
    interval :: (Int, Int)
  } -> Expression

  IntegerExpression :: {
    integer :: Integer,
    interval :: (Int, Int)
  } -> Expression

  RationalExpression :: {
    rational :: Rational,
    interval :: (Int, Int)
  } -> Expression

  ArrayExpression :: {
    open :: Token,
    elems :: [Expression],
    commas :: [Token],
    close :: Token
  } -> Expression

  AccessExpression :: {
    array :: Expression,
    open :: Token,
    index :: Expression,
    close :: Token
  } -> Expression

  CallExpression :: {
    funId :: SymbolId,
    open :: Token,
    args :: [Expression],
    commas :: [Token],
    close :: Token
  } -> Expression

  UnaryExpression :: {
    unary :: UnaryOperator,
    operand :: Expression
  } -> Expression

  BinaryExpression :: {
    left :: Expression,
    binary :: BinaryOperator,
    right :: Expression
  } -> Expression

  ParenthesizedExpression :: {
    open :: Token,
    inner :: Expression,
    close :: Token
  } -> Expression

  deriving (Eq, Show, Read, Data)


data UnaryOperator
  = PlusOperator {interval :: (Int, Int)}
  | MinusOperator {interval :: (Int, Int)}
  deriving (Eq, Show, Read, Data)


data BinaryOperator
  = AddOperator {interval :: (Int, Int)}
  | SubtractOperator {interval :: (Int, Int)}
  | MultiplyOperator {interval :: (Int, Int)}
  | DivideOperator {interval :: (Int, Int)}
  | ModuloOperator {interval :: (Int, Int)}
  | EqualOperator {interval :: (Int, Int)}
  | NotEqualOperator {interval :: (Int, Int)}
  | LessOperator {interval :: (Int, Int)}
  | LessOrEqualOperator {interval :: (Int, Int)}
  | GreaterOperator {interval :: (Int, Int)}
  | GreaterOrEqualOperator {interval :: (Int, Int)}
  | AndOperator {interval :: (Int, Int)}
  | OrOperator {interval :: (Int, Int)}
  | XorOperator {interval :: (Int, Int)}
  | PlainAssignOperator {interval :: (Int, Int)}
  | AddAssignOperator {interval :: (Int, Int)}
  | SubtractAssignOperator {interval :: (Int, Int)}
  | MultiplyAssignOperator {interval :: (Int, Int)}
  | DivideAssignOperator {interval :: (Int, Int)}
  | ModuloAssignOperator {interval :: (Int, Int)}
  deriving (Eq, Show, Read, Data)


data SymbolId = SymbolId {name :: String, interval :: (Int, Int)}
  deriving (Eq, Show, Read, Data)


data TypeId
  = PlainTypeId {name :: String, interval :: (Int, Int)}
  | ArrayTypeId {open :: Token, innerTypeId :: TypeId, close :: Token}
  deriving (Eq, Show, Read, Data)


newtype Token = Token {interval :: (Int, Int)}
  deriving (Eq, Show, Read, Data)


instance Interval Definition where
  start :: Num a => Definition -> a
  start VarDefinition {varKeyword} = start varKeyword
  start FunDefinition {defKeyword} = start defKeyword


  end :: Num a => Definition -> a
  end VarDefinition {semicolon} = end semicolon
  end FunDefinition {body} = end body


instance Interval Statement where
  start :: Num a => Statement -> a
  start DefinitionStatement {definition} = start definition
  start ExpressionStatement {value} = start value
  start IfStatement {ifKeyword} = start ifKeyword
  start IfElseStatement {ifKeyword} = start ifKeyword
  start WhileStatement {whileKeyword} = start whileKeyword
  start DoWhileStatement {doKeyword} = start doKeyword
  start ReturnStatement {returnKeyword} = start returnKeyword
  start AssertStatement {assertKeyword} = start assertKeyword
  start BreakpointStatement {breakpointKeyword} = start breakpointKeyword
  start BlockStatement {open} = start open


  end :: Num a => Statement -> a
  end DefinitionStatement {definition} = end definition
  end ExpressionStatement {semicolon} = end semicolon
  end IfStatement {trueBranch} = end trueBranch
  end IfElseStatement {falseBranch} = end falseBranch
  end WhileStatement {body} = end body
  end DoWhileStatement {semicolon} = end semicolon
  end ReturnStatement {semicolon} = end semicolon
  end AssertStatement {semicolon} = end semicolon
  end BreakpointStatement {semicolon} = end semicolon
  end BlockStatement {close} = end close


instance Interval Expression where
  start :: Num a => Expression -> a
  start VarExpression {interval} = start interval
  start IntegerExpression {interval} = start interval
  start RationalExpression {interval} = start interval
  start ArrayExpression {open} = start open
  start AccessExpression {array} = start array
  start CallExpression {funId} = start funId
  start UnaryExpression {unary} = start unary
  start BinaryExpression {left} = start left
  start ParenthesizedExpression {open} = start open


  end :: Num a => Expression -> a
  end VarExpression {interval} = end interval
  end IntegerExpression {interval} = end interval
  end RationalExpression {interval} = end interval
  end ArrayExpression {close} = end close
  end AccessExpression {close} = end close
  end CallExpression {close} = end close
  end UnaryExpression {operand} = end operand
  end BinaryExpression {right} = end right
  end ParenthesizedExpression {close} = end close


instance Interval UnaryOperator where
  start :: Num a => UnaryOperator -> a
  start PlusOperator {interval} = start interval
  start MinusOperator {interval} = start interval


  end :: Num a => UnaryOperator -> a
  end PlusOperator {interval} = end interval
  end MinusOperator {interval} = end interval


instance Interval BinaryOperator where
  start :: Num a => BinaryOperator -> a
  start AddOperator {interval} = start interval
  start SubtractOperator {interval} = start interval
  start MultiplyOperator {interval} = start interval
  start DivideOperator {interval} = start interval
  start ModuloOperator {interval} = start interval
  start EqualOperator {interval} = start interval
  start NotEqualOperator {interval} = start interval
  start LessOperator {interval} = start interval
  start LessOrEqualOperator {interval} = start interval
  start GreaterOperator {interval} = start interval
  start GreaterOrEqualOperator {interval} = start interval
  start AndOperator {interval} = start interval
  start OrOperator {interval} = start interval
  start XorOperator {interval} = start interval
  start PlainAssignOperator {interval} = start interval
  start AddAssignOperator {interval} = start interval
  start SubtractAssignOperator {interval} = start interval
  start MultiplyAssignOperator {interval} = start interval
  start DivideAssignOperator {interval} = start interval
  start ModuloAssignOperator {interval} = start interval


  end :: Num a => BinaryOperator -> a
  end AddOperator {interval} = end interval
  end SubtractOperator {interval} = end interval
  end MultiplyOperator {interval} = end interval
  end DivideOperator {interval} = end interval
  end ModuloOperator {interval} = end interval
  end EqualOperator {interval} = end interval
  end NotEqualOperator {interval} = end interval
  end LessOperator {interval} = end interval
  end LessOrEqualOperator {interval} = end interval
  end GreaterOperator {interval} = end interval
  end GreaterOrEqualOperator {interval} = end interval
  end AndOperator {interval} = end interval
  end OrOperator {interval} = end interval
  end XorOperator {interval} = end interval
  end PlainAssignOperator {interval} = end interval
  end AddAssignOperator {interval} = end interval
  end SubtractAssignOperator {interval} = end interval
  end MultiplyAssignOperator {interval} = end interval
  end DivideAssignOperator {interval} = end interval
  end ModuloAssignOperator {interval} = end interval


instance Interval SymbolId where
  start :: Num a => SymbolId -> a
  start SymbolId {interval} = start interval


  end :: Num a => SymbolId -> a
  end SymbolId {interval} = end interval


instance Interval TypeId where
  start :: Num a => TypeId -> a
  start PlainTypeId {interval} = start interval
  start ArrayTypeId {open} = start open


  end :: Num a => TypeId -> a
  end PlainTypeId {interval} = end interval
  end ArrayTypeId {close} = end close


instance Interval Token where
  start :: Num a => Token -> a
  start Token {interval} = start interval


  end :: Num a => Token -> a
  end Token {interval} = end interval


instance Display Expression where
  displays :: Expression -> ShowS
  displays = \case
    VarExpression {varName} ->
      showString varName

    IntegerExpression {integer} ->
      shows integer

    RationalExpression {rational} ->
      showsRatio rational

    AccessExpression {array, index} ->
      displays array .
      showChar '[' .
      displays index .
      showChar ']'

    ParenthesizedExpression {inner} ->
      showChar '(' .
      displays inner .
      showChar ')'

    UnaryExpression {unary = PlusOperator {}, operand} ->
      showChar '+' .
      displays operand

    UnaryExpression {unary = MinusOperator {}, operand} ->
      showChar '-' .
      displays operand

    BinaryExpression {left, binary, right} ->
      displays left .
      showChar ' ' .
      displays binary .
      showChar ' ' .
      displays right

    ArrayExpression {elems = []} ->
      showString "[]"

    ArrayExpression {elems = elem : elems} ->
      showChar '[' .
      displays elem .
      go elems

      where
        go [] = showChar ']'
        go (elem : elems) = showString ", " . displays elem . go elems

    CallExpression {funId = SymbolId {name}, args = []} ->
      showString name .
      showString "()"

    CallExpression {funId = SymbolId {name}, args = arg : args} ->
      showString name .
      showChar '(' .
      displays arg .
      go args

      where
        go [] = showChar ')'
        go (arg : args) = showString ", " . displays arg . go args


instance Display UnaryOperator where
  displays :: UnaryOperator -> ShowS
  displays PlusOperator {} = showChar '+'
  displays MinusOperator {} = showChar '-'


instance Display BinaryOperator where
  displays :: BinaryOperator -> ShowS
  displays AddOperator {} = showChar '+'
  displays SubtractOperator {} = showChar '-'
  displays MultiplyOperator {} = showChar '*'
  displays DivideOperator {} = showChar '/'
  displays ModuloOperator {} = showChar '%'
  displays EqualOperator {} = showString "=="
  displays NotEqualOperator {} = showString "!="
  displays LessOperator {} = showChar '<'
  displays LessOrEqualOperator {} = showString "<="
  displays GreaterOperator {} = showChar '>'
  displays GreaterOrEqualOperator {} = showString ">="
  displays AndOperator {} = showString "and"
  displays OrOperator {} = showString "or"
  displays XorOperator {} = showString "xor"
  displays PlainAssignOperator {} = showChar '='
  displays AddAssignOperator {} = showString "+="
  displays SubtractAssignOperator {} = showString "-="
  displays MultiplyAssignOperator {} = showString "*="
  displays DivideAssignOperator {} = showString "/="
  displays ModuloAssignOperator {} = showString "%="
