{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Devin.Typers (
  checkDevin,
  checkDeclaration,
  checkStatement,
  checkExpression
) where

import Control.Monad
import Data.Foldable
import Data.Traversable

import Devin.Error
import Devin.Syntax
import Devin.Type
import Devin.Typer


checkDevin :: Devin -> Typer ()
checkDevin Devin {declarations} = do
  for_ declarations checkDeclaration1
  for_ declarations checkDeclaration2


checkDeclaration :: Declaration -> Typer ()
checkDeclaration declaration = do
  checkDeclaration1 declaration
  checkDeclaration2 declaration


checkDeclaration1 :: Declaration -> Typer ()
checkDeclaration1 = \case
  VariableDeclaration {} -> pure ()

  FunctionDeclaration {functionId = SymbolId {name}, parameters, returnInfo} -> do
    parameterTs <- for parameters $ \(_, _, typeInfo) -> case typeInfo of
      Just (_, parameterTypeId) -> getType parameterTypeId
      Nothing -> pure Unknown

    returnT <- case returnInfo of
      Just (_, returnTypeId) -> getType returnTypeId
      Nothing -> pure Unknown

    defineFunctionSignature name (parameterTs, returnT)


checkDeclaration2 :: Declaration -> Typer ()
checkDeclaration2 = \case
  VariableDeclaration {variableId = SymbolId {name}, value} -> do
    t <- checkExpression value
    defineVariableType name t

  FunctionDeclaration {parameters, returnInfo, body} -> withNewScope $ do
    for_ parameters $ \(_, SymbolId {name}, typeInfo) -> case typeInfo of
      Just (_, parameterTypeId) -> do
        parameterT <- getType parameterTypeId
        defineVariableType name parameterT

      Nothing -> defineVariableType name Unknown

    returnT <- case returnInfo of
      Just (_, returnTypeId) -> getType returnTypeId
      Nothing -> pure Unknown

    checkStatement returnT body
    pure ()


checkStatement :: Type -> Statement -> Typer Bool
checkStatement expectedT statement = case statement of
  DeclarationStatement {declaration = VariableDeclaration {variableId = SymbolId {name}, value}} -> do
    t <- checkExpression value
    defineVariableType name t
    pure False

  DeclarationStatement {declaration} -> do
    checkDeclaration declaration
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
    trueBranchReturns <- withNewScope (checkStatement expectedT trueBranch)
    falseBranchReturns <- withNewScope (checkStatement expectedT falseBranch)
    pure (trueBranchReturns && falseBranchReturns)

  WhileStatement {predicate, body} -> do
    t <- checkExpression predicate
    unless (t <: Bool) (report (InvalidType predicate Bool t))
    withNewScope (checkStatement expectedT body)
    pure False

  DoWhileStatement {body, predicate} -> do
    returns <- withNewScope (checkStatement expectedT body)
    t <- checkExpression predicate
    unless (t <: Bool) (report (InvalidType predicate Bool t))
    pure returns

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

  DebugStatement {} -> pure False

  BlockStatement {statements} -> withNewScope $ do
    for_ statements $ \case
      DeclarationStatement {declaration} -> checkDeclaration1 declaration
      _ -> pure ()

    foldlM (\returns statement -> (returns ||) <$> check statement) False statements

    where
      check DeclarationStatement {declaration} = do {checkDeclaration2 declaration; pure False}
      check statement = checkStatement expectedT statement


checkExpression :: Expression -> Typer Type
checkExpression expression = case expression of
  IntegerExpression {} -> pure Int

  RationalExpression {} -> pure Float

  VariableExpression {variableName, interval} ->
    lookupVariableType variableName >>= \case
      Just (t, _) -> pure t
      Nothing -> report' (UnknownVariable variableName interval)

  ArrayExpression {elements = []} -> pure (Array Unknown)

  ArrayExpression {elements = element : elements} -> go elements =<< checkExpression element
    where
      go elements Unknown = do
        for_ elements checkExpression
        pure (Array Unknown)

      go [] t = pure (Array t)

      go (element : elements) t = checkExpression element >>= \case
        Unknown -> do
          for_ elements checkExpression
          pure (Array Unknown)

        t' | t' <: t -> go elements t'

        _ -> do
          for_ elements checkExpression
          pure (Array Any)

  AccessExpression {array, index} -> do
    arrayT <- checkExpression array
    indexT <- checkExpression index

    case (arrayT, indexT) of
      (Array t, Int) -> pure t
      (Array _, _) -> report' (InvalidType index Int indexT)
      (_, _) -> report' (InvalidType array (Array Unknown) arrayT)

  CallExpression {functionId = SymbolId {name, interval}, arguments} ->
    lookupFunctionSignature name >>= \case
      Just ((parameterTs, returnT), _) -> go 0 arguments parameterTs
        where
          go _ [] [] = pure returnT

          go n (argument : arguments) (parameterT : parameterTs) = do
            argumentT <- checkExpression argument
            unless (argumentT <: parameterT) (report (InvalidType argument parameterT argumentT))
            go (n + 1) arguments parameterTs

          go n arguments parameterTs =
            report' (InvalidArgumentCount expression (n + length parameterTs) (n + length arguments))

      Nothing -> report' (UnknownFunction name interval)

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

  UnaryExpression {unary, operand} | NotOperator {} <- unary -> do
    operandT <- checkExpression operand

    case operandT of
      Unknown -> pure Unknown
      Bool -> pure Bool
      _ -> report' (InvalidUnary unary operandT)

  UnaryExpression {unary, operand} | LenOperator {} <- unary -> do
    operandT <- checkExpression operand

    case operandT of
      Unknown -> pure Unknown
      Array _ -> pure Int
      _ -> report' (InvalidUnary unary operandT)

  BinaryExpression {left, binary, right} | AddOperator {} <- binary -> do
    leftT <- checkExpression left
    rightT <- checkExpression right

    case (leftT, rightT) of
      (Unknown, _) -> pure Unknown
      (_, Unknown) -> pure Unknown
      (Int, Int) -> pure Int
      (Float, Float) -> pure Float
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

  BinaryExpression {left, binary, right} | EqualOperator {} <- binary -> do
    checkExpression left
    checkExpression right
    pure Bool

  BinaryExpression {left, binary, right} | NotEqualOperator {} <- binary -> do
    checkExpression left
    checkExpression right
    pure Bool

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

    if leftT <: rightT then
      pure leftT
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
  PlainTypeId {name, interval} ->
    lookupType name >>= \case
      Just (t, _) -> do
        pure t

      Nothing -> do
        report' (UnknownType name interval)
        defineType name (Placeholder name)

  ArrayTypeId {innerTypeId} -> do
    t <- getType innerTypeId
    pure (Array t)


report' :: Error -> Typer Type
report' error = do
  report error
  pure Unknown
