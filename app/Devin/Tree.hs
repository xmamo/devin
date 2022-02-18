{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Devin.Tree (
  Tree,
  devinTree,
  declarationTree,
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
import Data.Text qualified as Text

import Data.Vector ((!))
import Data.Vector qualified as Vector

import Data.Tree (Tree)
import Data.Tree qualified as Tree

import Devin.Evaluator
import Devin.Syntax
import Devin.Utils


devinTree :: Devin -> Tree (Text, Text)
devinTree Devin {declarations} = Tree.Node ("Devin", "") (map declarationTree declarations)


declarationTree :: Declaration -> Tree (Text, Text)
declarationTree = \case
  VariableDeclaration {varKeyword, variableId, equalSign, value, semicolon} ->
    Tree.Node ("VariableDeclaration", "") [
      tokenTree "var" varKeyword,
      symbolIdTree variableId,
      tokenTree "=" equalSign,
      expressionTree value,
      tokenTree ";" semicolon
    ]

  FunctionDeclaration {defKeyword, functionId, open, parameters, commas, close, returnInfo, body} ->
    Tree.Node ("FunctionDeclaration", "") $ concat [
      [tokenTree "def" defKeyword],
      [symbolIdTree functionId],
      [tokenTree "(" open],
      go parameters commas,
      [tokenTree ")" close],
      maybe [] (\(arrow, typeId) -> [tokenTree "->" arrow, typeIdTree typeId]) returnInfo,
      [statementTree body]
    ]
    where
      go ps [] = concatMap parameterTree ps
      go [] cs = map (tokenTree ",") cs
      go (p : ps) (c : cs) = parameterTree p ++ [tokenTree "," c] ++ go ps cs

      parameterTree (refKeyword, parameterId, parameterInfo) =
        concat [
          maybe [] (\token -> [tokenTree "ref" token]) refKeyword,
          [symbolIdTree parameterId],
          maybe [] (\(colon, t) -> [tokenTree ":" colon, typeIdTree t]) parameterInfo
        ]


statementTree :: Statement -> Tree (Text, Text)
statementTree = \case
  DeclarationStatement {declaration} ->
    Tree.Node ("DeclarationStatement", "") [declarationTree declaration]

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
  
  DebugStatement {debugKeyword, semicolon} ->
    Tree.Node ("DebugStatement", "") [
      tokenTree "debug" debugKeyword,
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

  VariableExpression {variableName} ->
    Tree.Node ("VariableExpression", Text.pack variableName) []

  ArrayExpression {open, elements = es, commas = cs, close} ->
    Tree.Node ("ArrayExpression", "") ([tokenTree "[" open] ++ go es cs ++ [tokenTree "]" close])
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

  CallExpression {functionId, open, arguments = as, commas = cs, close} ->
    Tree.Node ("CallExpression", "") $ concat [
      [symbolIdTree functionId, tokenTree "(" open],
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
binaryOperatorTree AddOperator {} = Tree.Node ("AddOperator", "+") []
binaryOperatorTree SubtractOperator {} = Tree.Node ("SubtractOperator", "-") []
binaryOperatorTree MultiplyOperator {} = Tree.Node ("MultiplyOperator", "*") []
binaryOperatorTree DivideOperator {} = Tree.Node ("DivideOperator", "/") []
binaryOperatorTree ModuloOperator {} = Tree.Node ("ModuloOperator", "%") []
binaryOperatorTree EqualOperator {} = Tree.Node ("EqualOperator", "==") []
binaryOperatorTree NotEqualOperator {} = Tree.Node ("NotEqualOperator", "!=") []
binaryOperatorTree LessOperator {} = Tree.Node ("LessOperator", "<") []
binaryOperatorTree LessOrEqualOperator {} = Tree.Node ("LessOrEqualOperator", "<=") []
binaryOperatorTree GreaterOperator {} = Tree.Node ("GreaterOperator", ">") []
binaryOperatorTree GreaterOrEqualOperator {} = Tree.Node ("GreaterOrEqualOperator", ">=") []
binaryOperatorTree AndOperator {} = Tree.Node ("AndOperator", "and") []
binaryOperatorTree OrOperator {} = Tree.Node ("OrOperator", "or") []
binaryOperatorTree XorOperator {} = Tree.Node ("XorOperator", "xor") []
binaryOperatorTree PlainAssignOperator {} = Tree.Node ("PlainAssignOperator", "=") []
binaryOperatorTree AddAssignOperator {} = Tree.Node ("AddAssignOperator", "+=") []
binaryOperatorTree SubtractAssignOperator {} = Tree.Node ("SubtractAssignOperator", "-=") []
binaryOperatorTree MultiplyAssignOperator {} = Tree.Node ("MultiplyAssignOperator", "*=") []
binaryOperatorTree DivideAssignOperator {} = Tree.Node ("DivideAssignOperator", "/=") []
binaryOperatorTree ModuloAssignOperator {} = Tree.Node ("ModuloAssignOperator", "%=") []


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


stateForest :: MonadIO m => State -> m [Tree (Text, Text)]
stateForest = \case
  [] -> pure []

  Frame {variables = []} : parents -> stateForest parents

  Frame {variables} : parents -> do
    subforest <- for variables $ \(name, r) -> do
      v <- readReference r
      s <- displayValue v
      pure (Tree.Node (Text.pack name, Text.pack s) [])

    forest <- stateForest parents
    pure (Tree.Node ("Frame", "") subforest : forest)


displayValue :: MonadIO m => Value -> m String
displayValue value = do
  s <- displaysValue value
  pure (s "")


displaysValue :: MonadIO m => Value -> m ShowS
displaysValue = \case
  Unit -> pure (showString "unit")
  Bool x -> pure (showString (if x then "true" else "false"))
  Int x -> pure (shows x)
  Float x -> pure (showFFloat Nothing x)

  Array rs | Vector.null rs -> pure (showString "[]")

  Array rs -> do
    x <- readReference (rs ! 0)
    s1 <- displaysValue x
    s2 <- go (Vector.length rs) 0
    pure (showChar '[' . s1 . s2)

    where
      go n i | i >= n = pure (showChar ']')

      go n i = do
        x <- readReference (rs ! i)
        s1 <- displaysValue x
        s2 <- go n (i + 1)
        pure (showString ", " . s1 . s2)
