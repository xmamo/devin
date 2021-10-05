module Typers (
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
import qualified Data.Map as Map

import CallTarget (CallTarget)
import qualified CallTarget
import qualified Syntax
import Type (Type)
import qualified Type
import Typer (Typer)
import qualified Typer
import qualified Typer.Error as Error
import qualified Unicode


checkDeclarations :: [Syntax.Declaration] -> Typer [Syntax.Declaration]
checkDeclarations declarations = do
  for_ declarations checkDeclaration1
  for declarations checkDeclaration2


checkDeclaration1 :: Syntax.Declaration -> Typer ()
checkDeclaration1 = \case
  Syntax.VariableDeclaration{} -> pure ()

  Syntax.FunctionDeclaration{functionId, parameters, returnInfo, body} -> do
    let parameterIds = parameters <&> (._1)
    parameterTypes <- for parameters (\p -> checkType p._3 <&> (.t))

    returnType <- case returnInfo of
      Just (_, typeId) -> do
        typeId' <- checkType typeId
        pure typeId'.t

      Nothing -> pure Type.Unit

    functions <- Typer.getFunctions

    let (head : tail) = functions
    let key = Unicode.collate functionId.name

    case head !? key of
      Just infos | any (liftEq Type.areCompatible parameterTypes . (._1)) infos ->
        Typer.report (Error.FunctionRedefinition functionId parameterTypes)

      Just infos ->
        let infos' = (parameterTypes, returnType, CallTarget.UserDefined parameterIds body) : infos
         in Typer.setFunctions (Map.insert key infos' head : tail)

      Nothing ->
        let infos' = [(parameterTypes, returnType, CallTarget.UserDefined parameterIds body)]
         in Typer.setFunctions (Map.insert key infos' head : tail)


checkDeclaration2 :: Syntax.Declaration -> Typer Syntax.Declaration
checkDeclaration2 declaration = case declaration of
  Syntax.VariableDeclaration{variableId, typeInfo, value} -> do
    typeInfo' <- case typeInfo of
      Just (colon, typeId) -> do
        typeId' <- checkType typeId
        pure (Just (colon, typeId'))

      Nothing -> pure Nothing

    value' <- checkExpression value

    variableId' <- case typeInfo' of
      Just (_, typeId') -> do
        let typeOk = Type.areCompatible value'.t typeId'.t
        unless typeOk (Typer.report (Error.InvalidType value typeId'.t value'.t))
        Typer.updateVariables (Map.insert (Unicode.collate variableId.name) typeId'.t)
        pure variableId{t = typeId'.t}

      Nothing -> do
        Typer.updateVariables (Map.insert (Unicode.collate variableId.name) value'.t)
        pure variableId{t = value'.t}

    pure declaration{variableId = variableId', typeInfo = typeInfo', value = value'}

  Syntax.FunctionDeclaration{functionId, parameters, returnInfo, body} -> do
    parameters' <- for parameters \(id, colon, typeId) -> do
      typeId' <- checkType typeId
      pure (id{t = typeId'.t}, colon, typeId')

    let locals = parameters' <&> (._1)

    (returnType, returnInfo') <- case returnInfo of
      Just (arrow, typeId) -> do
        typeId' <- checkType typeId
        pure (typeId'.t, Just (arrow, typeId'))

      Nothing -> pure (Type.Unit, Nothing)

    let functionId' = functionId{t = Type.Function (locals <&> (.t)) returnType}

    body' <- Typer.scoped do
      let f variables local = Map.insert (Unicode.collate local.name) local.t variables
      Typer.updateVariables \variables -> foldl' f variables locals
      Typer.updateFunctions (Map.empty :)
      checkStatement returnType body

    unless (Type.areCompatible returnType Type.Unit || Syntax.doesReturn body') $
      Typer.report (Error.MissingReturnPath functionId (locals <&> (.t)))

    pure declaration{functionId = functionId', parameters = parameters', returnInfo = returnInfo', body = body'}


checkStatement :: Type -> Syntax.Statement -> Typer Syntax.Statement
checkStatement expectedType statement = case statement of
  Syntax.ExpressionStatement{expression} -> do
    expression' <- checkExpression expression
    unless (Syntax.hasSideEffects expression) (Typer.report (Error.NoSideEffects statement))
    pure statement{expression = expression'}

  Syntax.IfStatement{predicate, trueBranch} -> do
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Bool
    unless predicateOk (Typer.report (Error.InvalidType predicate Type.Bool predicate'.t))
    trueBranch' <- Typer.scoped (checkStatement expectedType trueBranch)
    pure statement{predicate = predicate', trueBranch = trueBranch'}

  Syntax.IfElseStatement{predicate, trueBranch, falseBranch} -> do
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Bool
    unless predicateOk (Typer.report (Error.InvalidType predicate Type.Bool predicate'.t))
    trueBranch' <- Typer.scoped (checkStatement expectedType trueBranch)
    falseBranch' <- Typer.scoped (checkStatement expectedType falseBranch)
    pure statement{predicate = predicate', trueBranch = trueBranch', falseBranch = falseBranch'}

  Syntax.WhileStatement{predicate, body} -> do
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Bool
    unless predicateOk (Typer.report (Error.InvalidType predicate Type.Bool predicate'.t))
    body' <- Typer.scoped (checkStatement expectedType body)
    pure statement{predicate = predicate', body = body'}

  Syntax.DoWhileStatement{body, predicate} -> do
    body' <- checkStatement expectedType body
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Bool
    unless predicateOk (Typer.report (Error.InvalidType predicate Type.Bool predicate'.t))
    pure statement{body = body', predicate = predicate'}

  Syntax.ReturnStatement{result = Just result} -> do
    result' <- checkExpression result
    let resultOk = Type.areCompatible result'.t expectedType
    unless resultOk (Typer.report (Error.InvalidReturnType statement expectedType result'.t))
    pure statement{result = Just result'}

  Syntax.ReturnStatement{result = Nothing} -> do
    let expectedUnit = Type.areCompatible expectedType Type.Unit
    unless expectedUnit (Typer.report (Error.MissingReturnValue statement expectedType))
    pure statement

  Syntax.BlockStatement{elements} -> Typer.scoped do
    for_ elements \case
      Left declaration -> checkDeclaration1 declaration
      Right _ -> pure ()

    elements' <- for elements \case
      Left declaration -> Left <$> checkDeclaration2 declaration
      Right statement -> Right <$> checkStatement expectedType statement

    pure statement{elements = elements'}


checkExpression :: Syntax.Expression -> Typer Syntax.Expression
checkExpression expression = case expression of
  Syntax.IntegerExpression{} -> pure expression{t = Type.Int}

  Syntax.RationalExpression{} -> pure expression{t = Type.Float}

  Syntax.VariableExpression{variableId} -> do
    variableId' <- checkVariable variableId
    pure expression{variableId = variableId', t = variableId'.t}

  Syntax.CallExpression{targetId, arguments} -> do
    arguments' <- for arguments checkExpression
    (targetId', target') <- checkFunction targetId (arguments' <&> (.t))
    pure expression{targetId = targetId', arguments = arguments', target = target', t = targetId'.t.result}

  Syntax.UnaryExpression{unary, operand} -> do
    operand' <- checkExpression operand

    t <- case (unary, operand'.t) of
      (_, Type.Error) -> pure Type.Error
      (Syntax.PlusOperator{}, Type.Int) -> pure Type.Int
      (Syntax.PlusOperator{}, Type.Float) -> pure Type.Float
      (Syntax.MinusOperator{}, Type.Int) -> pure Type.Int
      (Syntax.MinusOperator{}, Type.Float) -> pure Type.Float
      (Syntax.NotOperator{}, Type.Bool) -> pure Type.Bool
      _ -> Typer.report (Error.InvalidUnary unary operand'.t) $> Type.Error

    let unary' = unary{t = Type.Function [operand'.t] t}
    pure expression{unary = unary', operand = operand', t}

  Syntax.BinaryExpression{left, binary, right} -> do
    left' <- checkExpression left
    right' <- checkExpression right

    t <- case (left'.t, binary, right'.t) of
      (Type.Error, _, _) -> pure Type.Error
      (_, _, Type.Error) -> pure Type.Error
      (Type.Int, Syntax.AddOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.AddOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.SubtractOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.SubtractOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.MultiplyOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.MultiplyOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.DivideOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.DivideOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.ModuloOperator{}, Type.Int) -> pure Type.Int
      (_, Syntax.EqualOperator{}, _) -> pure Type.Bool
      (_, Syntax.NotEqualOperator{}, _) -> pure Type.Bool
      (Type.Int, Syntax.LessOperator{}, Type.Int) -> pure Type.Bool
      (Type.Float, Syntax.LessOperator{}, Type.Float) -> pure Type.Bool
      (Type.Int, Syntax.LessOrEqualOperator{}, Type.Int) -> pure Type.Bool
      (Type.Float, Syntax.LessOrEqualOperator{}, Type.Float) -> pure Type.Bool
      (Type.Int, Syntax.GreaterOperator{}, Type.Int) -> pure Type.Bool
      (Type.Float, Syntax.GreaterOperator{}, Type.Float) -> pure Type.Bool
      (Type.Int, Syntax.GreaterOrEqualOperator{}, Type.Int) -> pure Type.Bool
      (Type.Float, Syntax.GreaterOrEqualOperator{}, Type.Float) -> pure Type.Bool
      (Type.Bool, Syntax.AndOperator{}, Type.Bool) -> pure Type.Bool
      (Type.Bool, Syntax.OrOperator{}, Type.Bool) -> pure Type.Bool
      _ -> Typer.report (Error.InvalidBinary binary left'.t right'.t) $> Type.Error

    let binary' = binary{t = Type.Function [left'.t, right'.t] t}
    pure expression{left = left', binary = binary', right = right', t}

  Syntax.AssignExpression{variableId, assign, value} -> do
    variableId' <- checkVariable variableId
    value' <- checkExpression value

    t <- case (variableId'.t, assign, value'.t) of
      (Type.Error, _, _) -> pure Type.Error
      (_, _, Type.Error) -> pure Type.Error
      (Type.Unit, Syntax.AssignOperator{}, Type.Unit) -> pure Type.Unit
      (Type.Bool, Syntax.AssignOperator{}, Type.Bool) -> pure Type.Bool
      (Type.Int, Syntax.AssignOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.AssignOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.AddAssignOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.AddAssignOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.SubtractAssignOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.SubtractAssignOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.MultiplyAssignOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.MultiplyAssignOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.DivideAssignOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.DivideAssignOperator{}, Type.Float) -> pure Type.Float
      (Type.Int, Syntax.RemainderAssignOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.RemainderAssignOperator{}, Type.Float) -> pure Type.Float
      _ -> Typer.report (Error.InvalidAssign assign variableId'.t value'.t) $> Type.Error

    let assign' = assign{t = Type.Function [variableId'.t, value'.t] t}
    pure expression{variableId = variableId', assign = assign', value = value', t = value'.t}

  Syntax.ParenthesizedExpression{inner} -> do
    inner' <- checkExpression inner
    pure expression{inner = inner', t = inner'.t}


checkType :: Syntax.Identifier -> Typer Syntax.Identifier
checkType typeId = do
  types <- Typer.getTypes
  let key = Unicode.collate typeId.name

  case types !? key of
    Just t -> pure typeId{t}

    Nothing -> do
      Typer.report (Error.UnknownType typeId)
      Typer.setTypes (Map.insert key (Type.Unknown typeId.name) types)
      pure typeId{t = Type.Unknown typeId.name}


checkVariable :: Syntax.Identifier -> Typer Syntax.Identifier
checkVariable variableId = do
  variables <- Typer.getVariables
  let key = Unicode.collate variableId.name

  case variables !? key of
    Just t -> pure variableId{t}

    Nothing -> do
      Typer.report (Error.UnknownVariable variableId)
      Typer.setVariables (Map.insert key Type.Error variables)
      pure variableId{t = Type.Error}


checkFunction :: Syntax.Identifier -> [Type] -> Typer (Syntax.Identifier, CallTarget)
checkFunction targetId parameterTypes = go =<< Typer.getFunctions
  where
    key = Unicode.collate targetId.name

    go [] = do
      Typer.updateFunctions \(head : tail) ->
        Map.insert key [(parameterTypes, Type.Error, undefined)] head : tail

      Typer.report (Error.UnknownFunction targetId parameterTypes)
      pure (targetId{t = Type.Function parameterTypes Type.Error}, undefined)

    go (head : tail) =
      case find (liftEq Type.areCompatible parameterTypes . (._1)) =<< head !? key of
        Just _ | Type.Error `elem` parameterTypes ->
          pure (targetId{t = Type.Function parameterTypes Type.Error}, undefined)

        Just (_, returnType, target) ->
          pure (targetId{t = Type.Function parameterTypes returnType}, target)

        Nothing -> go tail
