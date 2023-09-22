{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Highlight.Brackets (
  clearBracketsHighlighting,
  highlightDevinBrackets,
  highlightDefinitionBrackets,
  highlightStatementBrackets,
  highlightExpressionBrackets
) where

import Control.Monad.IO.Class

import Control.Monad.Extra

import qualified GI.Gtk as Gtk

import Devin.Interval
import Devin.Syntax

import Devin.Highlight


clearBracketsHighlighting ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Gtk.TextIter -> Gtk.TextIter -> m ()
clearBracketsHighlighting buffer tags =
  Gtk.textBufferRemoveTag buffer (bracketTag tags)


highlightDevinBrackets ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Gtk.TextIter -> Devin -> m Bool
highlightDevinBrackets buffer tags insertIter Devin{definitions} =
  anyM (highlightDefinitionBrackets buffer tags insertIter) definitions


highlightDefinitionBrackets ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Gtk.TextIter -> Definition -> m Bool
highlightDefinitionBrackets buffer tags insertIter = \case
  VarDefinition{value} ->
    highlightExpressionBrackets buffer tags insertIter value

  FunDefinition{open, close, body} ->
    orM [
      highlightBrackets buffer tags insertIter open close,
      highlightStatementBrackets buffer tags insertIter body
    ]


highlightStatementBrackets ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Gtk.TextIter -> Statement -> m Bool
highlightStatementBrackets buffer tags insertIter = \case
  ReturnStatement{result = Nothing} -> pure False
  BreakpointStatement{} -> pure False

  ExpressionStatement{value} ->
    highlightExpressionBrackets buffer tags insertIter value

  DefinitionStatement{definition} ->
    highlightDefinitionBrackets buffer tags insertIter definition

  IfStatement{predicate, trueBranch} ->
    orM [
      highlightExpressionBrackets buffer tags insertIter predicate,
      highlightStatementBrackets buffer tags insertIter trueBranch
    ]

  IfElseStatement{predicate, trueBranch, falseBranch} ->
    orM [
      highlightExpressionBrackets buffer tags insertIter predicate,
      highlightStatementBrackets buffer tags insertIter trueBranch,
      highlightStatementBrackets buffer tags insertIter falseBranch
    ]

  WhileStatement{predicate, body} ->
    orM [
      highlightExpressionBrackets buffer tags insertIter predicate,
      highlightStatementBrackets buffer tags insertIter body
    ]

  DoWhileStatement{body, predicate} ->
    orM [
      highlightStatementBrackets buffer tags insertIter body,
      highlightExpressionBrackets buffer tags insertIter predicate
    ]

  ReturnStatement{result = Just result} ->
    highlightExpressionBrackets buffer tags insertIter result

  AssertStatement{predicate} ->
    highlightExpressionBrackets buffer tags insertIter predicate

  BlockStatement{open, statements, close} ->
    orM [
      anyM (highlightStatementBrackets buffer tags insertIter) statements,
      highlightBrackets buffer tags insertIter open close
    ]


highlightExpressionBrackets ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  a -> Tags -> Gtk.TextIter -> Expression -> m Bool
highlightExpressionBrackets buffer tags insertIter = \case
  IntegerExpression{} -> pure False
  RationalExpression{} -> pure False
  VarExpression{} -> pure False

  ArrayExpression{open, elems, close} ->
    orM [
      anyM (highlightExpressionBrackets buffer tags insertIter) elems,
      highlightBrackets buffer tags insertIter open close
    ]

  AccessExpression{array, open, index, close} ->
    orM [
      highlightExpressionBrackets buffer tags insertIter array,
      highlightExpressionBrackets buffer tags insertIter index,
      highlightBrackets buffer tags insertIter open close
    ]

  CallExpression{open, args, close} ->
    orM [
      anyM (highlightExpressionBrackets buffer tags insertIter) args,
      highlightBrackets buffer tags insertIter open close
    ]

  UnaryExpression{operand} ->
    highlightExpressionBrackets buffer tags insertIter operand

  BinaryExpression{left, right} ->
    orM [
      highlightExpressionBrackets buffer tags insertIter left,
      highlightExpressionBrackets buffer tags insertIter right
    ]

  ParenthesizedExpression{open, inner, close} ->
    orM [
      highlightExpressionBrackets buffer tags insertIter inner,
      highlightBrackets buffer tags insertIter open close
    ]


highlightBrackets ::
  (Gtk.IsTextBuffer a, Interval b, Interval c, MonadIO m) =>
  a -> Tags -> Gtk.TextIter -> b -> c -> m Bool
highlightBrackets buffer tags insertIter open close = do
  openStartIter <- Gtk.textBufferGetIterAtOffset buffer (start open)
  openEndIter <- Gtk.textBufferGetIterAtOffset buffer (end open)
  closeStartIter <- Gtk.textBufferGetIterAtOffset buffer (start close)
  closeEndIter <- Gtk.textBufferGetIterAtOffset buffer (end close)

  applyParenthesisTag <- anyM (Gtk.textIterEqual insertIter)
    [openStartIter, openEndIter, closeStartIter, closeEndIter]

  if applyParenthesisTag then do
    Gtk.textBufferApplyTag buffer (bracketTag tags) openStartIter openEndIter
    Gtk.textBufferApplyTag buffer (bracketTag tags) closeStartIter closeEndIter
    pure True
  else
    pure False
