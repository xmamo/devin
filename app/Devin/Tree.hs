{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Devin.Tree (
  Tree,
  devinTree,
  definitionTree,
  statementTree,
  expressionTree,
  unaryOperatorTree,
  binaryOperatorTree,
  symbolIdTree,
  tokenTree,
  stateForest
) where

import Control.Monad.IO.Class
import Data.Traversable
import Numeric

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Vector ((!))
import qualified Data.Vector as Vector

import Data.Tree (Tree)
import qualified Data.Tree as Tree

import Devin.Evaluator
import Devin.Syntax
import Devin.Utils


devinTree :: Devin -> Tree (Text, Text)
devinTree Devin {definitions} =
  Tree.Node ("Devin", "") (map definitionTree definitions)


definitionTree :: Definition -> Tree (Text, Text)
definitionTree = \case
  VarDefinition {varKeyword, varId, equalSign, value, semicolon} ->
    Tree.Node ("VarDefinition", "") [
      tokenTree "var" varKeyword,
      symbolIdTree varId,
      tokenTree "=" equalSign,
      expressionTree value,
      tokenTree ";" semicolon
    ]

  FunDefinition {defKeyword, funId, open, params, commas, close, returnInfo, body} ->
    Tree.Node ("FunDefinition", "") $ concat [
      [tokenTree "def" defKeyword],
      [symbolIdTree funId],
      [tokenTree "(" open],
      go params commas,
      [tokenTree ")" close],
      maybe [] (\(arrow, id) -> [tokenTree "->" arrow, typeIdTree id]) returnInfo,
      [statementTree body]
    ]

    where
      go ps [] = concatMap paramTree ps
      go [] cs = map (tokenTree ",") cs
      go (p : ps) (c : cs) = paramTree p ++ [tokenTree "," c] ++ go ps cs

      paramTree (refKeyword, paramId, paramInfo) =
        concat [
          maybe [] (\token -> [tokenTree "ref" token]) refKeyword,
          [symbolIdTree paramId],
          maybe [] (\(colon, t) -> [tokenTree ":" colon, typeIdTree t]) paramInfo
        ]


statementTree :: Statement -> Tree (Text, Text)
statementTree = \case
  DefinitionStatement {definition} ->
    Tree.Node ("DefinitionStatement", "") [definitionTree definition]

  ExpressionStatement {value, semicolon} ->
    Tree.Node ("ExpressionStatement", "") [
      expressionTree value,
      tokenTree ";" semicolon
    ]

  IfStatement {ifKeyword, predicate, trueBranch} ->
    Tree.Node ("IfStatement", "") [
      tokenTree "if" ifKeyword,
      expressionTree predicate,
      statementTree trueBranch
    ]

  IfElseStatement {ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} ->
    Tree.Node ("IfElseStatement", "") [
      tokenTree "if" ifKeyword,
      expressionTree predicate,
      statementTree trueBranch,
      tokenTree "else" elseKeyword,
      statementTree falseBranch
    ]

  WhileStatement {whileKeyword, predicate, body} ->
    Tree.Node ("WhileStatement", "") [
      tokenTree "while" whileKeyword,
      expressionTree predicate,
      statementTree body
    ]

  DoWhileStatement {doKeyword, body, whileKeyword, predicate, semicolon} ->
    Tree.Node ("DoWhileStatement", "") [
      tokenTree "do" doKeyword,
      statementTree body,
      tokenTree "while" whileKeyword,
      expressionTree predicate,
      tokenTree ";" semicolon
    ]

  ReturnStatement {returnKeyword, result = Just result, semicolon} ->
    Tree.Node ("ReturnStatement", "") [
      tokenTree "return" returnKeyword,
      expressionTree result,
      tokenTree ";" semicolon
    ]

  AssertStatement {assertKeyword, predicate, semicolon} ->
    Tree.Node ("AssertStatement", "") [
      tokenTree "assert" assertKeyword,
      expressionTree predicate,
      tokenTree ";" semicolon
    ]

  ReturnStatement {returnKeyword, result = Nothing, semicolon} ->
    Tree.Node ("ReturnStatement", "") [
      tokenTree "return" returnKeyword,
      tokenTree ";" semicolon
    ]

  BreakpointStatement {breakpointKeyword, semicolon} ->
    Tree.Node ("BreakpointStatement", "") [
      tokenTree "breakpoint" breakpointKeyword,
      tokenTree ";" semicolon
    ]

  BlockStatement {open, statements, close} ->
    Tree.Node ("BlockStatement", "") $ concat [
      [tokenTree "{" open],
      map statementTree statements,
      [tokenTree "}" close]
    ]


expressionTree :: Expression -> Tree (Text, Text)
expressionTree = \case
  IntegerExpression {integer} ->
    Tree.Node ("IntegerExpression", Text.pack (show integer)) []

  RationalExpression {rational} ->
    Tree.Node ("RationalExpression", Text.pack (showRatio rational)) []

  VarExpression {varName} ->
    Tree.Node ("VarExpression", Text.pack varName) []

  ArrayExpression {open, elems = es, commas = cs, close} ->
    Tree.Node ("ArrayExpression", "") $ concat [
      [tokenTree "[" open],
      go es cs,
      [tokenTree "]" close]
    ]
    where
      go es [] = map expressionTree es
      go [] cs = map (tokenTree ",") cs
      go (e : es) (c : cs) = expressionTree e : tokenTree "," c : go es cs

  AccessExpression {array, open, index, close} ->
    Tree.Node ("AccessExpression", "") [
      expressionTree array,
      tokenTree "[" open,
      expressionTree index,
      tokenTree "]" close
    ]

  CallExpression {funId, open, args = as, commas = cs, close} ->
    Tree.Node ("CallExpression", "") $ concat [
      [symbolIdTree funId, tokenTree "(" open],
      go as cs,
      [tokenTree ")" close]
    ]
    where
      go as [] = map expressionTree as
      go [] cs = map (tokenTree ",") cs
      go (a : as) (c : cs) = expressionTree a : tokenTree "," c : go as cs

  UnaryExpression {unary, operand} ->
    Tree.Node ("UnaryExpression", "") [
      unaryOperatorTree unary,
      expressionTree operand
    ]

  BinaryExpression {left, binary, right} ->
    Tree.Node ("BinaryExpression", "") [
      expressionTree left,
      binaryOperatorTree binary,
      expressionTree right
    ]

  ParenthesizedExpression {open, inner, close} ->
    Tree.Node ("ParenthesizedExpression", "") [
      tokenTree "(" open,
      expressionTree inner,
      tokenTree ")" close
    ]


unaryOperatorTree :: UnaryOperator -> Tree (Text, Text)
unaryOperatorTree PlusOperator {} = Tree.Node ("PlusOperator", "+") []
unaryOperatorTree MinusOperator {} = Tree.Node ("MinusOperator", "-") []
unaryOperatorTree NotOperator {} = Tree.Node ("NotOperator", "not") []
unaryOperatorTree LenOperator {} = Tree.Node ("LenOperator", "len") []


binaryOperatorTree :: BinaryOperator -> Tree (Text, Text)
binaryOperatorTree = \case
  AddOperator {} -> Tree.Node ("AddOperator", "+") []
  SubtractOperator {} -> Tree.Node ("SubtractOperator", "-") []
  MultiplyOperator {} -> Tree.Node ("MultiplyOperator", "*") []
  DivideOperator {} -> Tree.Node ("DivideOperator", "/") []
  ModuloOperator {} -> Tree.Node ("ModuloOperator", "%") []
  EqualOperator {} -> Tree.Node ("EqualOperator", "==") []
  NotEqualOperator {} -> Tree.Node ("NotEqualOperator", "!=") []
  LessOperator {} -> Tree.Node ("LessOperator", "<") []
  LessOrEqualOperator {} -> Tree.Node ("LessOrEqualOperator", "<=") []
  GreaterOperator {} -> Tree.Node ("GreaterOperator", ">") []
  GreaterOrEqualOperator {} -> Tree.Node ("GreaterOrEqualOperator", ">=") []
  AndOperator {} -> Tree.Node ("AndOperator", "and") []
  OrOperator {} -> Tree.Node ("OrOperator", "or") []
  XorOperator {} -> Tree.Node ("XorOperator", "xor") []
  PlainAssignOperator {} -> Tree.Node ("PlainAssignOperator", "=") []
  AddAssignOperator {} -> Tree.Node ("AddAssignOperator", "+=") []
  SubtractAssignOperator {} -> Tree.Node ("SubtractAssignOperator", "-=") []
  MultiplyAssignOperator {} -> Tree.Node ("MultiplyAssignOperator", "*=") []
  DivideAssignOperator {} -> Tree.Node ("DivideAssignOperator", "/=") []
  ModuloAssignOperator {} -> Tree.Node ("ModuloAssignOperator", "%=") []


symbolIdTree :: SymbolId -> Tree (Text, Text)
symbolIdTree SymbolId {name} = Tree.Node ("SymbolId", Text.pack name) []


typeIdTree :: TypeId -> Tree (Text, Text)
typeIdTree = \case
  PlainTypeId {name} ->
    Tree.Node ("PlainTypeId", Text.pack name) []

  ArrayTypeId {open, innerTypeId, close} ->
    Tree.Node ("ArrayTypeId", "") [
      tokenTree "[" open,
      typeIdTree innerTypeId,
      tokenTree "]" close
    ]


tokenTree :: String -> Token -> Tree (Text, Text)
tokenTree label Token {} = Tree.Node ("Token", Text.pack label) []


stateForest :: MonadIO m => State -> m (Tree.Forest (Text, Text))
stateForest = \case
  [] -> pure []

  Frame {vars = []} : parents -> stateForest parents

  Frame {vars} : parents -> do
    subforest <- for vars $ \(name, r) -> do
      v <- readRef r
      s <- displayVal v
      pure (Tree.Node (Text.pack name, Text.pack s) [])

    forest <- stateForest parents
    pure (Tree.Node ("Frame", "") subforest : forest)


displayVal :: MonadIO m => Value -> m String
displayVal val = do
  s <- displaysVal val
  pure (s "")


displaysVal :: MonadIO m => Value -> m ShowS
displaysVal = \case
  Unit -> pure (showString "unit")
  Bool x -> pure (showString (if x then "true" else "false"))
  Int x -> pure (shows x)
  Float x -> pure (showFFloat Nothing x)

  Array rs | Vector.null rs -> pure (showString "[]")

  Array rs -> do
    x <- readRef (rs ! 0)
    s1 <- displaysVal x
    s2 <- go (Vector.length rs) 1
    pure (showChar '[' . s1 . s2)

    where
      go n i | i >= n = pure (showChar ']')

      go n i = do
        x <- readRef (rs ! i)
        s1 <- displaysVal x
        s2 <- go n (i + 1)
        pure (showString ", " . s1 . s2)
