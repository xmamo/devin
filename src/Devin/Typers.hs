{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Typers (
  checkDevin,
  checkDefinition,
  checkStatement,
  checkExpression
) where

import Control.Monad
import Data.Foldable
import Data.Traversable

import Control.Monad.Extra

import Devin.Error
import Devin.Syntax
import Devin.Type
import Devin.Typer


checkDevin :: Devin -> Typer ()
checkDevin Devin {definitions} = do
  for_ definitions checkDefinition1
  for_ definitions checkDefinition2


checkDefinition :: Definition -> Typer ()
checkDefinition definition = do
  checkDefinition1 definition
  checkDefinition2 definition


checkDefinition1 :: Definition -> Typer ()
checkDefinition1 = \case
  VarDefinition {} -> pure ()

  FunDefinition {funId = SymbolId {name}, params, returnInfo} -> do
    paramTs <- for params $ \(_, _, typeInfo) -> case typeInfo of
      Just (_, paramTypeId) -> getType paramTypeId
      Nothing -> pure Unknown

    returnT <- case returnInfo of
      Just (_, returnTypeId) -> getType returnTypeId
      Nothing -> pure Unknown

    defineFunSignature name (paramTs, returnT)


checkDefinition2 :: Definition -> Typer ()
checkDefinition2 = \case
  VarDefinition {varId = SymbolId {name}, value} -> do
    t <- checkExpression value
    defineVarType name t

  FunDefinition {funId, params, returnInfo, body} -> withNewScope $ do
    for_ params $ \(_, SymbolId {name}, typeInfo) -> case typeInfo of
      Just (_, paramTypeId) -> do
        paramT <- getType paramTypeId
        defineVarType name paramT

      Nothing -> defineVarType name Unknown

    returnT <- case returnInfo of
      Just (_, returnTypeId) -> getType returnTypeId
      Nothing -> pure Unknown

    case returnT of
      Unknown -> void (checkStatement Unknown body)
      Unit -> void (checkStatement Unit body)

      _ -> do
        doesReturn <- checkStatement returnT body
        unless doesReturn (report (MissingReturnStatement funId))


checkStatement :: Type -> Statement -> Typer Bool
checkStatement expectedT statement = case statement of
  DefinitionStatement {definition} -> do
    checkDefinition definition
    pure False

  ExpressionStatement {value} -> do
    checkExpression value
    pure False

  IfStatement {predicate, trueBranch} -> do
    t <- checkExpression predicate
    unless (t <: Bool) (report (InvalidType predicate Bool t))
    withNewScope (checkStatement expectedT trueBranch)
    pure False

  IfElseStatement {predicate, trueBranch, falseBranch} -> do
    t <- checkExpression predicate
    unless (t <: Bool) (report (InvalidType predicate Bool t))
    trueBranchDoesReturn <- withNewScope (checkStatement expectedT trueBranch)
    falseBranchDoesReturn <- withNewScope (checkStatement expectedT falseBranch)
    pure (trueBranchDoesReturn && falseBranchDoesReturn)

  WhileStatement {predicate, body} -> do
    t <- checkExpression predicate
    unless (t <: Bool) (report (InvalidType predicate Bool t))
    withNewScope (checkStatement expectedT body)
    pure False

  DoWhileStatement {body, predicate} -> do
    doesReturn <- withNewScope (checkStatement expectedT body)
    t <- checkExpression predicate
    unless (t <: Bool) (report (InvalidType predicate Bool t))
    pure doesReturn

  ReturnStatement {result = Just result} -> do
    t <- checkExpression result
    unless (t <: expectedT) (report (InvalidType result expectedT t))
    pure True

  ReturnStatement {result = Nothing} -> do
    unless (Unit <: expectedT) (report (MissingReturnValue statement expectedT))
    pure True

  AssertStatement {predicate} -> do
    t <- checkExpression predicate
    unless (t <: Bool) (report (InvalidType predicate Bool t))
    pure False

  BreakpointStatement {} -> pure False

  BlockStatement {statements} -> withNewScope $ do
    for_ statements $ \case
      DefinitionStatement {definition} -> checkDefinition1 definition
      _ -> pure ()

    foldlM f False statements

    where
      f doesReturn DefinitionStatement {definition} = do
        checkDefinition2 definition
        pure doesReturn

      f doesReturn statement = do
        doesReturn' <- checkStatement expectedT statement
        pure (doesReturn || doesReturn')


checkExpression :: Expression -> Typer Type
checkExpression expression = case expression of
  IntegerExpression {} -> pure Int

  RationalExpression {} -> pure Float

  VarExpression {varName, interval} -> lookupVarType varName >>= \case
    Just (t, _) -> pure t
    Nothing -> report' (UnknownVar varName interval)

  ArrayExpression {elems = []} -> pure (Array Unknown)

  ArrayExpression {elems = elem : elems} -> do
    t <- checkExpression elem

    flip loopM elems $ \case
      [] -> pure (Right (Array t))

      elem : elems -> checkExpression elem >>= \case
        Unknown -> do
          for_ elems checkExpression
          pure (Right (Array Unknown))

        t' | t' <: t -> pure (Left elems)

        t' -> do
          report (InvalidType elem t t')
          pure (Left elems)

  AccessExpression {array, index} -> do
    arrayT <- checkExpression array
    indexT <- checkExpression index

    case (arrayT, indexT) of
      (Unknown, Unknown) -> pure Unknown
      (Unknown, Int) -> pure Unknown
      (Unknown, _) -> report' (InvalidType index Int indexT)
      (Array t, Unknown) -> pure t
      (Array t, Int) -> pure t
      (Array _, _) -> report' (InvalidType index Int indexT)
      (_, _) -> report' (InvalidType array (Array Unknown) arrayT)

  CallExpression {funId = SymbolId {name, interval}, args} ->
    lookupFunSignature name >>= \case
      Just ((paramTs, returnT), _) -> go 0 args paramTs
        where
          go _ [] [] = pure returnT

          go n (arg : args) (paramT : paramTs) = do
            argT <- checkExpression arg
            unless (argT <: paramT) (report (InvalidType arg paramT argT))
            go (n + 1) args paramTs

          go n args paramTs = do
            let expected = n + length paramTs
            let actual = n + length args
            report' (InvalidArgCount expression expected actual)

      Nothing -> report' (UnknownFun name interval)

  UnaryExpression {unary, operand} | PlusOperator {} <- unary -> do
    operandT <- checkExpression operand

    case operandT of
      Unknown -> pure Unknown
      Int -> pure Int
      Float -> pure Float
      _ -> report' (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | MinusOperator {} <- unary -> do
    operandT <- checkExpression operand

    case operandT of
      Unknown -> pure Unknown
      Int -> pure Int
      Float -> pure Float
      _ -> report' (InvalidUnary unary operandT)

  BinaryExpression {left, binary, right} | AddOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
      (Array t1, Array t2) | Just t <- merge t1 t2 -> pure (Array t)
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | SubtractOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | MultiplyOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
      (Array t, Int) -> pure (Array t)
      (Int, Array t) -> pure (Array t)
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | DivideOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | ModuloOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary = EqualOperator {}, right} -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      _ -> pure Bool

  BinaryExpression {left, binary = NotEqualOperator {}, right} -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      _ -> pure Bool

  BinaryExpression {left, binary, right} | LessOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Bool
      (Float, Float) -> pure Bool
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | LessOrEqualOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Bool
      (Float, Float) -> pure Bool
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | GreaterOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Bool
      (Float, Float) -> pure Bool
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | GreaterOrEqualOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Bool
      (Float, Float) -> pure Bool
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | AndOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Bool, Bool) -> pure Bool
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | OrOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Bool, Bool) -> pure Bool
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | XorOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Bool, Bool) -> pure Bool
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | PlainAssignOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    if rightT <: leftT then
      pure rightT
    else
      report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | AddAssignOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | SubtractAssignOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | MultiplyAssignOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
      (Array t, Int) -> pure (Array t)
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | DivideAssignOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  BinaryExpression {left, binary, right} | ModuloAssignOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (_, _) -> report' (InvalidBinary binary leftT rightT)

  ParenthesizedExpression {inner} -> checkExpression inner


getType :: TypeId -> Typer Type
getType = \case
  PlainTypeId {name, interval} -> lookupType name >>= \case
    Just (t, _) -> pure t

    Nothing -> do
      report (UnknownType name interval)
      defineType name (Placeholder name)

  ArrayTypeId {innerTypeId} -> do
    t <- getType innerTypeId
    pure (Array t)


report' :: Error -> Typer Type
report' error = do
  report error
  pure Unknown
