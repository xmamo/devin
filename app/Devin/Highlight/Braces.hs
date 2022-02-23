{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Highlight.Braces (
  highlightDevinBraces,
  highlightDeclarationBraces,
  highlightStatementBraces,
  highlightExpressionBraces
) where

import Control.Monad.IO.Class

import Control.Monad.Extra

import Devin.Interval
import Devin.Syntax

import qualified Data.GI.Gtk as Gtk

import Devin.Highlight


highlightDevinBraces ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  Tags -> a -> Gtk.TextIter -> Devin -> m Bool
highlightDevinBraces tag buffer insertIter Devin{declarations} =
  anyM (highlightDeclarationBraces tag buffer insertIter) declarations


highlightDeclarationBraces ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  Tags -> a -> Gtk.TextIter -> Declaration -> m Bool
highlightDeclarationBraces tag buffer insertIter declaration = case declaration of
  VariableDeclaration {value} ->
    highlightExpressionBraces tag buffer insertIter value

  FunctionDeclaration {open, close, body} ->
    orM [
      highlightBraces tag buffer insertIter open close,
      highlightStatementBraces tag buffer insertIter body
    ]


highlightStatementBraces ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  Tags -> a -> Gtk.TextIter -> Statement -> m Bool
highlightStatementBraces tag buffer insertIter statement = case statement of
  ExpressionStatement {value} ->
    highlightExpressionBraces tag buffer insertIter value

  DeclarationStatement {declaration} ->
    highlightDeclarationBraces tag buffer insertIter declaration

  IfStatement {predicate, trueBranch} ->
    orM [
      highlightExpressionBraces tag buffer insertIter predicate,
      highlightStatementBraces tag buffer insertIter trueBranch
    ]

  IfElseStatement {predicate, trueBranch, falseBranch} ->
    orM [
      highlightExpressionBraces tag buffer insertIter predicate,
      highlightStatementBraces tag buffer insertIter trueBranch,
      highlightStatementBraces tag buffer insertIter falseBranch
    ]

  WhileStatement {predicate, body} ->
    orM [
      highlightExpressionBraces tag buffer insertIter predicate,
      highlightStatementBraces tag buffer insertIter body
    ]

  DoWhileStatement {body, predicate} ->
    orM [
      highlightStatementBraces tag buffer insertIter body,
      highlightExpressionBraces tag buffer insertIter predicate
    ]

  ReturnStatement {result = Nothing} ->
    pure False

  ReturnStatement {result = Just result} ->
    highlightExpressionBraces tag buffer insertIter result

  AssertStatement {predicate} ->
    highlightExpressionBraces tag buffer insertIter predicate

  DebugStatement {} ->
    pure False

  BlockStatement {open, statements, close} ->
    orM [
      anyM (highlightStatementBraces tag buffer insertIter) statements,
      highlightBraces tag buffer insertIter open close
    ]


highlightExpressionBraces ::
  (Gtk.IsTextBuffer a, MonadIO m) =>
  Tags -> a -> Gtk.TextIter -> Expression -> m Bool
highlightExpressionBraces tag buffer insertIter expression = case expression of
  IntegerExpression {} -> pure False
  RationalExpression {} -> pure False
  VariableExpression {} -> pure False

  ArrayExpression {open, elements, close} ->
    orM [
      anyM (highlightExpressionBraces tag buffer insertIter) elements,
      highlightBraces tag buffer insertIter open close
    ]

  AccessExpression {array, open, index, close} ->
    orM [
      highlightExpressionBraces tag buffer insertIter array,
      highlightExpressionBraces tag buffer insertIter index,
      highlightBraces tag buffer insertIter open close
    ]

  CallExpression {open, arguments, close} ->
    orM [
      anyM (highlightExpressionBraces tag buffer insertIter) arguments,
      highlightBraces tag buffer insertIter open close
    ]

  UnaryExpression {operand} ->
    highlightExpressionBraces tag buffer insertIter operand

  BinaryExpression {left, right} ->
    orM [
      highlightExpressionBraces tag buffer insertIter left,
      highlightExpressionBraces tag buffer insertIter right
    ]

  ParenthesizedExpression {open, inner, close} ->
    orM [
      highlightExpressionBraces tag buffer insertIter inner,
      highlightBraces tag buffer insertIter open close
    ]


highlightBraces ::
  (Gtk.IsTextBuffer a, Interval b, Interval c, MonadIO m) =>
  Tags -> a -> Gtk.TextIter -> b -> c -> m Bool
highlightBraces tags buffer insertIter open close = do
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
