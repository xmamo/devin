{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Highlight.Syntax (
  clearSyntaxHighlighting,
  highlightDevin,
  highlightDefinition,
  highlightStatement,
  highlightExpression,
) where

import Control.Monad.IO.Class
import Data.Foldable

import Control.Monad.Extra

import qualified GI.Gtk as Gtk

import Devin.Syntax

import Devin.Highlight


clearSyntaxHighlighting ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Gtk.TextIter -> Gtk.TextIter -> m ()
clearSyntaxHighlighting buffer tags startIter endIter = do
  Gtk.textBufferRemoveTag buffer (keywordTag tags) startIter endIter
  Gtk.textBufferRemoveTag buffer (varIdTag tags) startIter endIter
  Gtk.textBufferRemoveTag buffer (funIdTag tags) startIter endIter
  Gtk.textBufferRemoveTag buffer (typeTag tags) startIter endIter
  Gtk.textBufferRemoveTag buffer (numberTag tags) startIter endIter
  Gtk.textBufferRemoveTag buffer (operatorTag tags) startIter endIter
  Gtk.textBufferRemoveTag buffer (commentTag tags) startIter endIter
  Gtk.textBufferRemoveTag buffer (errorTag tags) startIter endIter


highlightDevin :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Tags -> Devin -> m ()
highlightDevin buffer tags Devin{definitions} =
  for_ definitions (highlightDefinition buffer tags)


highlightDefinition ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Definition -> m ()
highlightDefinition buffer tags = \case
  VarDefinition{varKeyword, varId, value} -> do
    highlightInterval (keywordTag tags) buffer varKeyword
    highlightInterval (varIdTag tags) buffer varId
    highlightExpression buffer tags value

  FunDefinition{defKeyword, funId, params, returnInfo, body} -> do
    highlightInterval (keywordTag tags) buffer defKeyword
    highlightInterval (funIdTag tags) buffer funId

    for_ params $ \(refKeyword, paramId, paramInfo) -> do
      whenJust refKeyword (highlightInterval (keywordTag tags) buffer)
      highlightInterval (varIdTag tags) buffer paramId
      whenJust paramInfo $ \(_, id) -> highlightInterval (typeTag tags) buffer id

    whenJust returnInfo $ \(_, id) -> highlightInterval (typeTag tags) buffer id
    highlightStatement buffer tags body


highlightStatement ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Statement -> m ()
highlightStatement buffer tags = \case
  DefinitionStatement{definition} ->
    highlightDefinition buffer tags definition

  ExpressionStatement{effect} ->
    highlightExpression buffer tags effect

  IfStatement{ifKeyword, predicate, trueBranch} -> do
    highlightInterval (keywordTag tags) buffer ifKeyword
    highlightExpression buffer tags predicate
    highlightStatement buffer tags trueBranch

  IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    highlightInterval (keywordTag tags) buffer ifKeyword
    highlightExpression buffer tags predicate
    highlightStatement buffer tags trueBranch
    highlightInterval (keywordTag tags) buffer elseKeyword
    highlightStatement buffer tags falseBranch

  WhileStatement{whileKeyword, predicate, body} -> do
    highlightInterval (keywordTag tags) buffer whileKeyword
    highlightExpression buffer tags predicate
    highlightStatement buffer tags body

  DoWhileStatement{doKeyword, body, whileKeyword, predicate} -> do
    highlightInterval (keywordTag tags) buffer doKeyword
    highlightStatement buffer tags body
    highlightInterval (keywordTag tags) buffer whileKeyword
    highlightExpression buffer tags predicate

  ReturnStatement{returnKeyword, result} -> do
    highlightInterval (keywordTag tags) buffer returnKeyword
    whenJust result (highlightExpression buffer tags)

  AssertStatement{assertKeyword, predicate} -> do
    highlightInterval (keywordTag tags) buffer assertKeyword
    highlightExpression buffer tags predicate

  BreakpointStatement{breakpointKeyword} ->
    highlightInterval (keywordTag tags) buffer breakpointKeyword

  BlockStatement{statements} ->
    for_ statements (highlightStatement buffer tags)


highlightExpression ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Expression -> m ()
highlightExpression buffer tags expression = case expression of
  IntegerExpression{} ->
    highlightInterval (numberTag tags) buffer expression

  RationalExpression{} ->
    highlightInterval (numberTag tags) buffer expression

  VarExpression{} ->
    highlightInterval (varIdTag tags) buffer expression

  ArrayExpression{elems} ->
    for_ elems (highlightExpression buffer tags)

  AccessExpression{array, index} -> do
    highlightExpression buffer tags array
    highlightExpression buffer tags index

  CallExpression{funId, args} -> do
    highlightInterval (funIdTag tags) buffer funId
    for_ args (highlightExpression buffer tags)

  UnaryExpression{unary, operand} -> do
    highlightInterval (operatorTag tags) buffer unary
    highlightExpression buffer tags operand

  BinaryExpression{left, binary, right} -> do
    highlightExpression buffer tags left
    highlightInterval (operatorTag tags) buffer binary
    highlightExpression buffer tags right

  ParenthesizedExpression{inner} ->
    highlightExpression buffer tags inner
