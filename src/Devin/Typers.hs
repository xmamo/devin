module Devin.Typers (
  checkDevin,
  checkDeclarations,
  checkStatement,
  checkVariable
) where

import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
import Data.Traversable

import Data.Map ((!?))

import Devin.CallTarget (CallTarget)
import qualified Devin.CallTarget as CallTarget
import Devin.Range
import Devin.Syntax
import Devin.Type
import Devin.Typer
import Devin.Typer.Error


checkDevin :: Devin -> Typer Devin
checkDevin devin = push $ do
  declarations' <- checkDeclarations devin.declarations
  pure devin{declarations = declarations'}


checkDeclarations :: Traversable t => t Declaration -> Typer (t Declaration)
checkDeclarations declarations = do
  declarations' <- for declarations checkDeclaration1
  for declarations' checkDeclaration2


checkDeclaration1 :: Declaration -> Typer Declaration
checkDeclaration1 declaration = case declaration of
  VariableDeclaration{} -> pure declaration

  FunctionDeclaration{functionId, parameters, returnInfo} -> do
    parameters' <- for parameters $ \(id, colon, typeId) -> do
      typeId' <- checkType typeId
      pure (id{t = typeId'.t}, colon, typeId')

    returnInfo' <- case returnInfo of
      Just (arrow, typeId) -> do
        typeId' <- checkType typeId
        pure (Just (arrow, typeId'))

      Nothing -> pure Nothing

    let parameterTypes = parameters' <&> (._3.t)
    let returnType = maybe Unit (._2.t) returnInfo'
    let functionId' = functionId{t = Function parameterTypes returnType}
    let callTarget = CallTarget.UserDefined (start declaration)
    defineFunction functionId' callTarget

    depth <- getDepth
    pure declaration{functionId = functionId', parameters = parameters', returnInfo = returnInfo', depth}


checkDeclaration2 :: Declaration -> Typer Declaration
checkDeclaration2 declaration = case declaration of
  VariableDeclaration{variableId, right} -> do
    right' <- checkExpression right
    let variableId' = variableId{t = right'.t}
    defineVariable variableId'
    pure declaration{variableId = variableId', right = right'}

  FunctionDeclaration{functionId, parameters, returnInfo, body} -> do
    let parameterIds = parameters <&> (._1)
    let parameterTypes = parameters <&> (._3.t)
    let returnType = maybe Unit (._2.t) returnInfo

    body' <- push $ do
      for_ parameterIds defineVariable
      checkStatement returnType body

    unless (areCompatible returnType Unit || doesReturn body') $
      report (MissingReturnPath functionId parameterTypes)

    pure declaration{body = body'}


checkStatement :: Type -> Statement -> Typer Statement
checkStatement expectedType statement = case statement of
  ExpressionStatement{value} -> do
    value' <- checkExpression value
    pure statement{value = value'}

  DeclarationStatement{declaration} -> do
    checkDeclaration1 declaration
    declaration' <- checkDeclaration2 declaration
    pure statement{declaration = declaration'}

  IfStatement{predicate, trueBranch} -> do
    predicate' <- checkExpression predicate
    let predicateOk = areCompatible predicate'.t Bool
    unless predicateOk (report (InvalidType predicate Bool predicate'.t))
    trueBranch' <- push (checkStatement expectedType trueBranch)
    pure statement{predicate = predicate', trueBranch = trueBranch'}

  IfElseStatement{predicate, trueBranch, falseBranch} -> do
    predicate' <- checkExpression predicate
    let predicateOk = areCompatible predicate'.t Bool
    unless predicateOk (report (InvalidType predicate Bool predicate'.t))
    trueBranch' <- push (checkStatement expectedType trueBranch)
    falseBranch' <- push (checkStatement expectedType falseBranch)
    pure statement{predicate = predicate', trueBranch = trueBranch', falseBranch = falseBranch'}

  WhileStatement{predicate, body} -> do
    predicate' <- checkExpression predicate
    let predicateOk = areCompatible predicate'.t Bool
    unless predicateOk (report (InvalidType predicate Bool predicate'.t))
    body' <- push (checkStatement expectedType body)
    pure statement{predicate = predicate', body = body'}

  DoWhileStatement{body, predicate} -> do
    body' <- push (checkStatement expectedType body)
    predicate' <- checkExpression predicate
    let predicateOk = areCompatible predicate'.t Bool
    unless predicateOk (report (InvalidType predicate Bool predicate'.t))
    pure statement{body = body', predicate = predicate'}

  ReturnStatement{result = Just result} -> do
    result' <- checkExpression result
    let resultOk = areCompatible result'.t expectedType
    unless resultOk (report (InvalidReturnType statement expectedType result'.t))
    pure statement{result = Just result'}

  ReturnStatement{result = Nothing} -> do
    let expectedUnit = areCompatible expectedType Unit
    unless expectedUnit (report (MissingReturnValue statement expectedType))
    pure statement

  BlockStatement{statements} -> push $ do
    statements' <- for statements $ \statement -> case statement of
      DeclarationStatement{declaration} -> do
        declaration' <- checkDeclaration1 declaration
        pure statement{declaration = declaration'}

      _ -> pure statement

    statements'' <- for statements' $ \statement -> case statement of
      DeclarationStatement{declaration} -> do
        declaration' <- checkDeclaration2 declaration
        pure statement{declaration = declaration'}

      _ -> pure statement

    statements''' <- for statements'' $ \statement -> case statement of
      DeclarationStatement{} -> pure statement
      _ -> checkStatement expectedType statement

    pure statement{statements = statements'''}


checkExpression :: Expression -> Typer Expression
checkExpression expression = case expression of
  IntegerExpression{} -> pure expression{t = Int}

  RationalExpression{} -> pure expression{t = Float}

  VariableExpression{variableId} -> do
    variableId' <- checkVariable variableId
    pure expression{variableId = variableId', t = variableId'.t}

  CallExpression{targetId, arguments} -> do
    depth <- getDepth
    arguments' <- for arguments checkExpression
    (targetId', target') <- checkFunction targetId (arguments' <&> (.t))
    pure expression{targetId = targetId', arguments = arguments', depth, target = target', t = targetId'.t.returnType}

  UnaryExpression{unary, operand} -> do
    operand' <- checkExpression operand

    t <- case (unary, operand'.t) of
      (_, Error) -> pure Error
      (PlusOperator{}, Int) -> pure Int
      (PlusOperator{}, Float) -> pure Float
      (MinusOperator{}, Int) -> pure Int
      (MinusOperator{}, Float) -> pure Float
      (NotOperator{}, Bool) -> pure Bool
      _ -> report (InvalidUnary unary operand'.t) $> Error

    let unary' = unary{t = Function [operand'.t] t}
    pure expression{unary = unary', operand = operand', t}

  BinaryExpression{left, binary, right} -> do
    left' <- checkExpression left
    right' <- checkExpression right

    t <- case (left'.t, binary, right'.t) of
      (Error, _, _) -> pure Error
      (_, _, Error) -> pure Error
      (Int, AddOperator{}, Int) -> pure Int
      (Float, AddOperator{}, Float) -> pure Float
      (Int, SubtractOperator{}, Int) -> pure Int
      (Float, SubtractOperator{}, Float) -> pure Float
      (Int, MultiplyOperator{}, Int) -> pure Int
      (Float, MultiplyOperator{}, Float) -> pure Float
      (Int, DivideOperator{}, Int) -> pure Int
      (Float, DivideOperator{}, Float) -> pure Float
      (Int, ModuloOperator{}, Int) -> pure Int
      (_, EqualOperator{}, _) -> pure Bool
      (_, NotEqualOperator{}, _) -> pure Bool
      (Int, LessOperator{}, Int) -> pure Bool
      (Float, LessOperator{}, Float) -> pure Bool
      (Int, LessOrEqualOperator{}, Int) -> pure Bool
      (Float, LessOrEqualOperator{}, Float) -> pure Bool
      (Int, GreaterOperator{}, Int) -> pure Bool
      (Float, GreaterOperator{}, Float) -> pure Bool
      (Int, GreaterOrEqualOperator{}, Int) -> pure Bool
      (Float, GreaterOrEqualOperator{}, Float) -> pure Bool
      (Bool, AndOperator{}, Bool) -> pure Bool
      (Bool, OrOperator{}, Bool) -> pure Bool
      _ -> report (InvalidBinary binary left'.t right'.t) $> Error

    let binary' = binary{t = Function [left'.t, right'.t] t}
    pure expression{left = left', binary = binary', right = right', t}

  AssignExpression{variableId, assign, right} -> do
    variableId' <- checkVariable variableId
    right' <- checkExpression right

    t <- case (variableId'.t, assign, right'.t) of
      (Error, _, _) -> pure Error
      (_, _, Error) -> pure Error
      (Unit, AssignOperator{}, Unit) -> pure Unit
      (Bool, AssignOperator{}, Bool) -> pure Bool
      (Int, AssignOperator{}, Int) -> pure Int
      (Float, AssignOperator{}, Float) -> pure Float
      (Int, AddAssignOperator{}, Int) -> pure Int
      (Float, AddAssignOperator{}, Float) -> pure Float
      (Int, SubtractAssignOperator{}, Int) -> pure Int
      (Float, SubtractAssignOperator{}, Float) -> pure Float
      (Int, MultiplyAssignOperator{}, Int) -> pure Int
      (Float, MultiplyAssignOperator{}, Float) -> pure Float
      (Int, DivideAssignOperator{}, Int) -> pure Int
      (Float, DivideAssignOperator{}, Float) -> pure Float
      (Int, ModuloAssignOperator{}, Int) -> pure Int
      (Float, ModuloAssignOperator{}, Float) -> pure Float
      _ -> report (InvalidAssign assign variableId'.t right'.t) $> Error

    let assign' = assign{t = Function [variableId'.t, right'.t] t}
    pure expression{variableId = variableId', assign = assign', right = right', t = right'.t}

  ParenthesizedExpression{inner} -> do
    inner' <- checkExpression inner
    pure expression{inner = inner', t = inner'.t}


checkType :: Identifier -> Typer Identifier
checkType typeId = do
  types <- getTypes

  case types !? typeId.name of
    Just t -> pure typeId{t}

    Nothing -> do
      report (UnknownType typeId)
      let typeId' = typeId{t = Unknown typeId.name}
      defineType typeId'
      pure typeId'


checkVariable :: Identifier -> Typer Identifier
checkVariable variableId = do
  variables <- getVariables

  case variables !? variableId.name of
    Just t -> pure variableId{t}

    Nothing -> do
      report (UnknownVariable variableId)
      let variableId' = variableId{t = Error}
      defineVariable variableId'
      pure variableId'


checkFunction :: Identifier -> [Type] -> Typer (Identifier, CallTarget)
checkFunction targetId parameterTypes = go =<< getFunctions
  where
    go [] = do
      report (UnknownFunction targetId parameterTypes)
      let targetId' = targetId{t = Function parameterTypes Error}
      defineFunction targetId' CallTarget.Undefined
      pure (targetId', CallTarget.Undefined)

    go (current : parents) = do
      let info = current !? targetId.name

      case find (\i -> liftEq areCompatible parameterTypes i._1) =<< info of
        Just _ | Error `elem` parameterTypes ->
          pure (targetId{t = Function parameterTypes Error}, CallTarget.Undefined)

        Just (_, returnType, target) ->
          pure (targetId{t = Function parameterTypes returnType}, target)

        Nothing -> go parents
