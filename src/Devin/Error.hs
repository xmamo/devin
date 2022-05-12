{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif

module Devin.Error (Error (..)) where

import Data.Data
import Data.Int

import Devin.Display
import Devin.Interval
import Devin.Syntax
import Devin.Type


data Error where
  UnknownVar :: {
    varName :: String,
    interval :: (Int, Int)
  } -> Error

  UnknownFun :: {
    funName :: String,
    interval :: (Int, Int)
  } -> Error

  UnknownType :: {
    typeName :: String,
    interval :: (Int, Int)
  } -> Error

  InvalidUnary :: {
    unary :: UnaryOperator,
    operandT :: Type
  } -> Error

  InvalidBinary :: {
    binary :: BinaryOperator,
    leftT :: Type,
    rightT :: Type
  } -> Error

  InvalidType :: {
    expression :: Expression,
    expectedT :: Type,
    actualT :: Type
  } -> Error

  -- Static errors:

  MissingReturnValue :: {
    statement :: Statement,
    expectedT :: Type
  } -> Error

  MissingReturnStatement :: {
    funId :: SymbolId
  } -> Error

  -- Runtime errors:

  IntegerOverflow :: {
    expression :: Expression
  } -> Error

  DivisionByZero :: {
    expression :: Expression
  } -> Error

  IndexOutOfBounds :: {
    expression :: Expression,
    value :: Int64
  } -> Error

  InvalidArgCount :: {
    expression :: Expression,
    expected :: Int,
    actual :: Int
  } -> Error

  AssertionFailed :: {
    statement :: Statement
  } -> Error

  deriving (Show, Read, Data)


instance Interval Error where
  start :: Num a => Error -> a
  start UnknownVar {interval} = start interval
  start UnknownFun {interval} = start interval
  start UnknownType {interval} = start interval
  start InvalidUnary {unary} = start unary
  start InvalidBinary {binary} = start binary
  start InvalidType {expression} = start expression
  start MissingReturnValue {statement} = start statement
  start MissingReturnStatement {funId} = start funId
  start IntegerOverflow {expression} = start expression
  start DivisionByZero {expression} = start expression
  start IndexOutOfBounds {expression} = start expression
  start InvalidArgCount {expression} = start expression
  start AssertionFailed {statement} = start statement


  end :: Num a => Error -> a
  end UnknownVar {interval} = end interval
  end UnknownFun {interval} = end interval
  end UnknownType {interval} = end interval
  end InvalidUnary {unary} = end unary
  end InvalidBinary {binary} = end binary
  end InvalidType {expression} = end expression
  end MissingReturnValue {statement} = end statement
  end MissingReturnStatement {funId} = end funId
  end IntegerOverflow {expression} = end expression
  end DivisionByZero {expression} = end expression
  end IndexOutOfBounds {expression} = end expression
  end InvalidArgCount {expression} = end expression
  end AssertionFailed {statement} = end statement


instance Display Error where
  displays :: Error -> ShowS
  displays = \case
    UnknownVar {varName} ->
      showString "Unknown variable: " .
      showString varName

    UnknownFun {funName} ->
      showString "Unknown function: " .
      showString funName

    UnknownType {typeName} ->
      showString "Unknown type: " .
      showString typeName

    InvalidUnary {unary, operandT} ->
      showString "Can’t apply “" .
      displays unary .
      showString "” to " .
      displays operandT

    InvalidBinary {binary, leftT, rightT} ->
      showString "Can’t apply “" .
      displays binary .
      showString "” to " .
      displays leftT .
      showString " and " .
      displays rightT

    InvalidType {expectedT, actualT} ->
      showString "Invalid type: expected " .
      displays expectedT .
      showString ", but got " .
      displays actualT

    MissingReturnValue {} ->
      showString "Missing return value"

    MissingReturnStatement {} ->
      showString "Missing return statement"

    IntegerOverflow {} ->
      showString "Integer overflow"

    DivisionByZero {} ->
      showString "Division by zero"

    IndexOutOfBounds {value} ->
      showString "Index out of bounds: " .
      shows value

    InvalidArgCount {expected, actual} ->
      showString "Invalid argument count: expected " .
      shows expected .
      showString " arguments, but got " .
      shows actual

    AssertionFailed {statement} | AssertStatement {predicate} <- statement ->
      showString "Assertion failed: " .
      displays predicate

    AssertionFailed {} ->
      showString "Assertion failed"
