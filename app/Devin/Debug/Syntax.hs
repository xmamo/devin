{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Devin.Debug.Syntax (
  devinTree,
  definitionTree,
  statementTree,
  expressionTree,
  unaryOperatorTree,
  binaryOperatorTree,
  symbolIdTree,
  tokenTree
) where

import Data.Tree

import qualified Data.Text as Text
import Data.Text (Text)

import Devin.Ratio
import Devin.Syntax


devinTree :: Devin -> Tree (Text, Text)
devinTree Devin{definitions} =
  Node ("Devin", "") (map definitionTree definitions)


definitionTree :: Definition -> Tree (Text, Text)
definitionTree = \case
  VarDefinition{varKeyword, varId, equalSign, value, semicolon} ->
    Node ("VarDefinition", "") [
      tokenTree "var" varKeyword,
      symbolIdTree varId,
      tokenTree "=" equalSign,
      expressionTree value,
      tokenTree ";" semicolon
    ]

  FunDefinition{defKeyword, funId, open, params, commas, close, returnInfo, body} ->
    Node ("FunDefinition", "") $ concat [
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
statementTree statement = case statement of
  DefinitionStatement{definition} ->
    Node ("DefinitionStatement", "") [definitionTree definition]

  ExpressionStatement{effect, semicolon} ->
    Node ("ExpressionStatement", "") [
      expressionTree effect,
      tokenTree ";" semicolon
    ]

  IfStatement{ifKeyword, predicate, trueBranch} ->
    Node ("IfStatement", "") [
      tokenTree "if" ifKeyword,
      expressionTree predicate,
      statementTree trueBranch
    ]

  IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} ->
    Node ("IfElseStatement", "") [
      tokenTree "if" ifKeyword,
      expressionTree predicate,
      statementTree trueBranch,
      tokenTree "else" elseKeyword,
      statementTree falseBranch
    ]

  WhileStatement{whileKeyword, predicate, body} ->
    Node ("WhileStatement", "") [
      tokenTree "while" whileKeyword,
      expressionTree predicate,
      statementTree body
    ]

  DoWhileStatement{doKeyword, body, whileKeyword, predicate, semicolon} ->
    Node ("DoWhileStatement", "") [
      tokenTree "do" doKeyword,
      statementTree body,
      tokenTree "while" whileKeyword,
      expressionTree predicate,
      tokenTree ";" semicolon
    ]

  ReturnStatement{returnKeyword, result = Just result, semicolon} ->
    Node ("ReturnStatement", "") [
      tokenTree "return" returnKeyword,
      expressionTree result,
      tokenTree ";" semicolon
    ]

  ReturnStatement{returnKeyword, result = Nothing, semicolon} ->
    Node ("ReturnStatement", "") [
      tokenTree "return" returnKeyword,
      tokenTree ";" semicolon
    ]

  AssertStatement{assertKeyword, predicate, semicolon} ->
    Node ("AssertStatement", "") [
      tokenTree "assert" assertKeyword,
      expressionTree predicate,
      tokenTree ";" semicolon
    ]

  BreakpointStatement{breakpointKeyword, semicolon} ->
    Node ("BreakpointStatement", "") [
      tokenTree "breakpoint" breakpointKeyword,
      tokenTree ";" semicolon
    ]

  BlockStatement{open, statements, close} ->
    Node ("BlockStatement", "") $ concat [
      [tokenTree "{" open],
      map statementTree statements,
      [tokenTree "}" close]
    ]


expressionTree :: Expression -> Tree (Text, Text)
expressionTree = \case
  IntegerExpression{integer} ->
    Node ("IntegerExpression", Text.pack (show integer)) []

  RationalExpression{rational} ->
    Node ("RationalExpression", Text.pack (showRatio rational)) []

  VarExpression{varName} ->
    Node ("VarExpression", Text.pack varName) []

  ArrayExpression{open, elems = es, commas = cs, close} ->
    Node ("ArrayExpression", "") $ concat [
      [tokenTree "[" open],
      go es cs,
      [tokenTree "]" close]
    ]

    where
      go es [] = map expressionTree es
      go [] cs = map (tokenTree ",") cs
      go (e : es) (c : cs) = expressionTree e : tokenTree "," c : go es cs

  AccessExpression{array, open, index, close} ->
    Node ("AccessExpression", "") [
      expressionTree array,
      tokenTree "[" open,
      expressionTree index,
      tokenTree "]" close
    ]

  CallExpression{funId, open, args = as, commas = cs, close} ->
    Node ("CallExpression", "") $ concat [
      [symbolIdTree funId, tokenTree "(" open],
      go as cs,
      [tokenTree ")" close]
    ]

    where
      go as [] = map expressionTree as
      go [] cs = map (tokenTree ",") cs
      go (a : as) (c : cs) = expressionTree a : tokenTree "," c : go as cs

  UnaryExpression{unary, operand} ->
    Node ("UnaryExpression", "") [
      unaryOperatorTree unary,
      expressionTree operand
    ]

  BinaryExpression{left, binary, right} ->
    Node ("BinaryExpression", "") [
      expressionTree left,
      binaryOperatorTree binary,
      expressionTree right
    ]

  ParenthesizedExpression{open, inner, close} ->
    Node ("ParenthesizedExpression", "") [
      tokenTree "(" open,
      expressionTree inner,
      tokenTree ")" close
    ]


unaryOperatorTree :: UnaryOperator -> Tree (Text, Text)
unaryOperatorTree PlusOperator{} = Node ("PlusOperator", "+") []
unaryOperatorTree MinusOperator{} = Node ("MinusOperator", "-") []


binaryOperatorTree :: BinaryOperator -> Tree (Text, Text)
binaryOperatorTree = \case
  AddOperator{} -> Node ("AddOperator", "+") []
  SubtractOperator{} -> Node ("SubtractOperator", "-") []
  MultiplyOperator{} -> Node ("MultiplyOperator", "*") []
  DivideOperator{} -> Node ("DivideOperator", "/") []
  ModuloOperator{} -> Node ("ModuloOperator", "%") []
  EqualOperator{} -> Node ("EqualOperator", "==") []
  NotEqualOperator{} -> Node ("NotEqualOperator", "!=") []
  LessOperator{} -> Node ("LessOperator", "<") []
  LessOrEqualOperator{} -> Node ("LessOrEqualOperator", "<=") []
  GreaterOperator{} -> Node ("GreaterOperator", ">") []
  GreaterOrEqualOperator{} -> Node ("GreaterOrEqualOperator", ">=") []
  AndOperator{} -> Node ("AndOperator", "and") []
  OrOperator{} -> Node ("OrOperator", "or") []
  XorOperator{} -> Node ("XorOperator", "xor") []
  PlainAssignOperator{} -> Node ("PlainAssignOperator", "=") []
  AddAssignOperator{} -> Node ("AddAssignOperator", "+=") []
  SubtractAssignOperator{} -> Node ("SubtractAssignOperator", "-=") []
  MultiplyAssignOperator{} -> Node ("MultiplyAssignOperator", "*=") []
  DivideAssignOperator{} -> Node ("DivideAssignOperator", "/=") []
  ModuloAssignOperator{} -> Node ("ModuloAssignOperator", "%=") []


symbolIdTree :: SymbolId -> Tree (Text, Text)
symbolIdTree SymbolId{name} = Node ("SymbolId", Text.pack name) []


typeIdTree :: TypeId -> Tree (Text, Text)
typeIdTree = \case
  PlainTypeId{name} ->
    Node ("PlainTypeId", Text.pack name) []

  ArrayTypeId{open, innerTypeId, close} ->
    Node ("ArrayTypeId", "") [
      tokenTree "[" open,
      typeIdTree innerTypeId,
      tokenTree "]" close
    ]


tokenTree :: String -> Token -> Tree (Text, Text)
tokenTree lexeme Token{} = Node ("Token", Text.pack lexeme) []
