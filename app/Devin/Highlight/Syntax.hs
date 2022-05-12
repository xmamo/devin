{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Highlight.Syntax (
  highlightDevin,
  highlightDefinition,
  highlightStatement,
  highlightExpression,
) where

import Control.Monad.IO.Class
import Data.Foldable

import Control.Monad.Extra

import Devin.Syntax

import qualified Data.GI.Gtk as Gtk

import Devin.Highlight


highlightDevin :: (Gtk.IsTextBuffer a, MonadIO m) => Tags -> a -> Devin -> m ()
highlightDevin tags buffer Devin {definitions} =
  for_ definitions (highlightDefinition tags buffer)


highlightDefinition ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  Tags -> a -> Definition -> m ()
highlightDefinition tags buffer = \case
  VarDefinition {varKeyword, varId, value} -> do
    highlightInterval (keywordTag tags) buffer varKeyword
    highlightInterval (varIdTag tags) buffer varId
    highlightExpression tags buffer value

  FunDefinition {defKeyword, funId, params, returnInfo, body} -> do
    highlightInterval (keywordTag tags) buffer defKeyword
    highlightInterval (funIdTag tags) buffer funId

    for_ params $ \(refKeyword, paramId, paramInfo) -> do
      whenJust refKeyword (highlightInterval (keywordTag tags) buffer)
      highlightInterval (varIdTag tags) buffer paramId
      whenJust paramInfo $ \(_, id) -> highlightInterval (typeTag tags) buffer id

    whenJust returnInfo $ \(_, id) -> highlightInterval (typeTag tags) buffer id
    highlightStatement tags buffer body


highlightStatement ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  Tags -> a -> Statement -> m ()
highlightStatement tags buffer = \case
  DefinitionStatement {definition} ->
    highlightDefinition tags buffer definition

  ExpressionStatement {value} ->
    highlightExpression tags buffer value

  IfStatement {ifKeyword, predicate, trueBranch} -> do
    highlightInterval (keywordTag tags) buffer ifKeyword
    highlightExpression tags buffer predicate
    highlightStatement tags buffer trueBranch

  IfElseStatement {ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    highlightInterval (keywordTag tags) buffer ifKeyword
    highlightExpression tags buffer predicate
    highlightStatement tags buffer trueBranch
    highlightInterval (keywordTag tags) buffer elseKeyword
    highlightStatement tags buffer falseBranch

  WhileStatement {whileKeyword, predicate, body} -> do
    highlightInterval (keywordTag tags) buffer whileKeyword
    highlightExpression tags buffer predicate
    highlightStatement tags buffer body

  DoWhileStatement {doKeyword, body, whileKeyword, predicate} -> do
    highlightInterval (keywordTag tags) buffer doKeyword
    highlightStatement tags buffer body
    highlightInterval (keywordTag tags) buffer whileKeyword
    highlightExpression tags buffer predicate

  ReturnStatement {returnKeyword, result} -> do
    highlightInterval (keywordTag tags) buffer returnKeyword
    whenJust result (highlightExpression tags buffer)

  AssertStatement {assertKeyword, predicate} -> do
    highlightInterval (keywordTag tags) buffer assertKeyword
    highlightExpression tags buffer predicate

  DebugStatement {debugKeyword} ->
    highlightInterval (keywordTag tags) buffer debugKeyword

  BlockStatement {statements} ->
    for_ statements (highlightStatement tags buffer)


highlightExpression ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  Tags -> a -> Expression -> m ()
highlightExpression tags buffer expression = case expression of
  IntegerExpression {} ->
    highlightInterval (numberTag tags) buffer expression

  RationalExpression {} ->
    highlightInterval (numberTag tags) buffer expression

  VarExpression {} ->
    highlightInterval (varIdTag tags) buffer expression

  ArrayExpression {elems} ->
    for_ elems (highlightExpression tags buffer)

  AccessExpression {array, index} -> do
    highlightExpression tags buffer array
    highlightExpression tags buffer index

  CallExpression {funId, args} -> do
    highlightInterval (funIdTag tags) buffer funId
    for_ args (highlightExpression tags buffer)

  UnaryExpression {unary, operand} -> do
    highlightInterval (operatorTag tags) buffer unary
    highlightExpression tags buffer operand

  BinaryExpression {left, binary, right} -> do
    highlightExpression tags buffer left
    highlightInterval (operatorTag tags) buffer binary
    highlightExpression tags buffer right

  ParenthesizedExpression {inner} ->
    highlightExpression tags buffer inner
