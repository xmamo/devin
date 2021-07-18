module Checkers (
  checkDeclarations,
  checkStatement,
  checkVariable
) where

import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
import Data.Traversable

import qualified Data.Map as Map

import Checker (Checker)
import qualified Checker
import qualified Error
import qualified Syntax
import Type (Type)
import qualified Type
import qualified Unicode


checkDeclarations :: [Syntax.Declaration ()] -> Checker [Syntax.Declaration Type]
checkDeclarations declarations = do
  for_ declarations checkDeclaration1
  for declarations checkDeclaration2


checkDeclaration1 :: Syntax.Declaration () -> Checker ()
checkDeclaration1 = \case
  Syntax.VariableDeclaration{} -> pure ()

  Syntax.FunctionDeclaration{functionId, parameters, returnInfo} -> do
    parameterTypes <- case parameters of
      Just (id, colon, typeId, rest) ->
        for ((undefined, id, colon, typeId) : rest) \(_, _, _, typeId) -> do
          typeId' <- checkType typeId
          pure typeId'.t

      Nothing -> pure []

    returnType <- case returnInfo of
      Just (_, typeId) -> do
        typeId' <- checkType typeId
        pure typeId'.t

      Nothing -> pure Type.Unit

    functions <- Checker.getFunctions
    let key = Unicode.collate functionId.name

    case functions of
      [] -> Checker.setFunctions [Map.singleton key [(parameterTypes, returnType)]]

      (head : tail) -> case Map.lookup key head of
        Just signatures | any (liftEq Type.areCompatible parameterTypes . fst) signatures ->
          Checker.report (Error.FunctionRedefinition functionId parameterTypes)

        Just signatures ->
          let signatures' = (parameterTypes, returnType) : signatures
          in Checker.setFunctions (Map.insert key signatures' head : tail)

        Nothing ->
          let signatures' = [(parameterTypes, returnType)]
          in Checker.setFunctions (Map.insert key signatures' head : tail)


checkDeclaration2 :: Syntax.Declaration () -> Checker (Syntax.Declaration Type)
checkDeclaration2 = \case
  Syntax.VariableDeclaration{varKeyword, variableId, typeInfo, equalSign, value, semicolon} -> do
    typeInfo' <- case typeInfo of
      Just (colon, typeId) -> do
        typeId' <- checkType typeId
        pure (Just (colon, typeId'))

      Nothing -> pure Nothing

    value' <- checkExpression value

    variableId' <- case typeInfo' of
      Just (_, typeId') -> do
        let typeOk = Type.areCompatible value'.t typeId'.t
        unless typeOk (Checker.report (Error.InvalidType value typeId'.t value'.t))
        Checker.updateVariables (Map.insert (Unicode.collate variableId.name) typeId'.t)
        pure (Syntax.Identifier variableId.s variableId.name typeId'.t)

      Nothing -> do
        Checker.updateVariables (Map.insert (Unicode.collate variableId.name) value'.t)
        pure (Syntax.Identifier variableId.s variableId.name value'.t)

    pure (Syntax.VariableDeclaration varKeyword variableId' typeInfo' equalSign value' semicolon)

  Syntax.FunctionDeclaration{defKeyword, functionId, open, parameters, close, returnInfo, body} -> do
    (locals, parameters') <- case parameters of
      Just (id, colon, typeId, rest) -> do
        typeId' <- checkType typeId

        rest' <- for rest \(comma, id, colon, typeId) -> do
          typeId' <- checkType typeId
          pure (comma, id $> typeId'.t, colon, typeId')

        let id' = id $> typeId'.t
        let locals = [id | (_, id, _, _) <- (undefined, id', undefined, typeId') : rest']
        pure (locals, Just (id', colon, typeId', rest'))

      Nothing -> pure ([], Nothing)

    (returnType, returnInfo') <- case returnInfo of
      Just (arrow, typeId) -> do
        typeId' <- checkType typeId
        pure (typeId'.t, Just (arrow, typeId'))

      Nothing -> pure (Type.Unit, Nothing)

    let t = Type.Function [local.t | local <- locals] returnType
        functionId' = Syntax.Identifier functionId.s functionId.name t

    body' <- Checker.scoped do
      let f variables local = Map.insert (Unicode.collate local.name) local.t variables
      Checker.updateVariables \variables -> foldl' f variables locals
      Checker.updateFunctions (Map.empty :)
      checkStatement returnType body

    unless (Type.areCompatible returnType Type.Unit || Syntax.doesReturn body') $
      Checker.report (Error.MissingReturnPath functionId [local.t | local <- locals])

    pure (Syntax.FunctionDeclaration defKeyword functionId' open parameters' close returnInfo' body')


checkStatement :: Type -> Syntax.Statement () -> Checker (Syntax.Statement Type)
checkStatement expectedType statement = case statement of
  Syntax.ExpressionStatement{value, semicolon} -> do
    value' <- checkExpression value
    unless (Syntax.hasSideEffects value) (Checker.report (Error.NoSideEffects statement))
    pure (Syntax.ExpressionStatement value' semicolon)

  Syntax.IfStatement{ifKeyword, predicate, trueBranch} -> do
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Bool
    unless predicateOk (Checker.report (Error.InvalidType predicate Type.Bool predicate'.t))
    statement' <- Checker.scoped (checkStatement expectedType trueBranch)
    pure (Syntax.IfStatement ifKeyword predicate' statement')

  Syntax.IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Bool
    unless predicateOk (Checker.report (Error.InvalidType predicate Type.Bool predicate'.t))
    trueBranch' <- Checker.scoped (checkStatement expectedType trueBranch)
    falseBranch' <- Checker.scoped (checkStatement expectedType falseBranch)
    pure (Syntax.IfElseStatement ifKeyword predicate' trueBranch' elseKeyword falseBranch')

  Syntax.WhileStatement{whileKeyword, predicate, body} -> do
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Bool
    unless predicateOk (Checker.report (Error.InvalidType predicate Type.Bool predicate'.t))
    statement' <- Checker.scoped (checkStatement expectedType body)
    pure (Syntax.WhileStatement whileKeyword predicate' statement')

  Syntax.DoWhileStatement{doKeyword, body, whileKeyword, predicate, semicolon} -> do
    statement' <- checkStatement expectedType body
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Bool
    unless predicateOk (Checker.report (Error.InvalidType predicate Type.Bool predicate'.t))
    pure (Syntax.DoWhileStatement doKeyword statement' whileKeyword predicate' semicolon)

  Syntax.ReturnStatement{returnKeyword, result = Just result, semicolon} -> do
    result' <- checkExpression result
    let resultOk = Type.areCompatible result'.t expectedType
    unless resultOk (Checker.report (Error.InvalidReturnType statement expectedType result'.t))
    pure (Syntax.ReturnStatement returnKeyword (Just result') semicolon)

  Syntax.ReturnStatement{returnKeyword, result = Nothing, semicolon} -> do
    let expectedUnit = Type.areCompatible expectedType Type.Unit
    unless expectedUnit (Checker.report (Error.MissingReturnValue statement expectedType))
    pure (Syntax.ReturnStatement returnKeyword Nothing semicolon)

  Syntax.BlockStatement{open, elements, close} -> Checker.scoped do
    for_ elements \case
      Left declaration -> checkDeclaration1 declaration
      Right _ -> pure ()

    elements' <- for elements \case
      Left declaration -> Left <$> checkDeclaration2 declaration
      Right statement -> Right <$> checkStatement expectedType statement

    pure (Syntax.BlockStatement open elements' close)


checkExpression :: Syntax.Expression () -> Checker (Syntax.Expression Type)
checkExpression expression = case expression of
  Syntax.IntegerExpression{} -> pure (expression $> Type.Int)

  Syntax.RationalExpression{} -> pure (expression $> Type.Float)

  Syntax.VariableExpression{variableId} -> do
    variableId' <- checkVariable variableId
    pure (Syntax.VariableExpression variableId' variableId'.t)

  Syntax.CallExpression{targetId, open, arguments = Just (first, rest), close} -> do
    first' <- checkExpression first
    rest' <- for rest \(comma, argument) -> (comma,) <$> checkExpression argument
    targetId' <- checkFunction targetId (first'.t : [argument.t | (_, argument) <- rest'])
    pure (Syntax.CallExpression targetId' open (Just (first', rest')) close targetId'.t.result)

  Syntax.CallExpression{targetId, open, arguments = Nothing, close} -> do
    targetId' <- checkFunction targetId []
    pure (Syntax.CallExpression targetId' open Nothing close targetId'.t.result)

  Syntax.UnaryExpression{unary, operand} -> do
    operand' <- checkExpression operand

    t <- case (unary, operand'.t) of
      (_, Type.Error) -> pure Type.Error
      (Syntax.PlusOperator{}, Type.Int) -> pure Type.Int
      (Syntax.PlusOperator{}, Type.Float) -> pure Type.Float
      (Syntax.MinusOperator{}, Type.Int) -> pure Type.Int
      (Syntax.MinusOperator{}, Type.Float) -> pure Type.Float
      (Syntax.NotOperator{}, Type.Bool) -> pure Type.Bool
      _ -> Checker.report (Error.InvalidUnary unary operand'.t) $> Type.Error

    let unary' = unary $> Type.Function [operand'.t] t
    pure (Syntax.UnaryExpression unary' operand' t)

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
      (Type.Int, Syntax.RemainderOperator{}, Type.Int) -> pure Type.Int
      (Type.Float, Syntax.RemainderOperator{}, Type.Float) -> pure Type.Float
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
      _ -> Checker.report (Error.InvalidBinary binary left'.t right'.t) $> Type.Error

    let binary' = binary $> Type.Function [left'.t, right'.t] t
    pure (Syntax.BinaryExpression left' binary' right' t)

  Syntax.AssignExpression{targetId, assign, value} -> do
    targetId' <- checkVariable targetId
    value' <- checkExpression value

    t <- case (targetId'.t, assign, value'.t) of
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
      _ -> Checker.report (Error.InvalidAssign assign targetId'.t value'.t) $> Type.Error

    let assign' = assign $> Type.Function [targetId'.t, value'.t] t
    pure (Syntax.AssignExpression targetId' assign' value' value'.t)

  Syntax.ParenthesizedExpression{open, inner, close} -> do
    inner' <- checkExpression inner
    pure (Syntax.ParenthesizedExpression open inner' close inner'.t)


checkType :: Syntax.Identifier () -> Checker (Syntax.Identifier Type)
checkType typeId@Syntax.Identifier{s, name} = do
  types <- Checker.getTypes
  let key = Unicode.collate typeId.name

  case Map.lookup key types of
    Just t -> pure (Syntax.Identifier s name t)

    Nothing -> do
      Checker.report (Error.UnknownType typeId)
      Checker.setTypes (Map.insert key (Type.Unknown name) types)
      pure (Syntax.Identifier s name (Type.Unknown name))


checkVariable :: Syntax.Identifier () -> Checker (Syntax.Identifier Type)
checkVariable variableId = do
  variables <- Checker.getVariables
  let key = Unicode.collate variableId.name

  case Map.lookup key variables of
    Just t -> pure (variableId $> t)

    Nothing -> do
      Checker.report (Error.UnknownVariable variableId)
      Checker.setVariables (Map.insert key Type.Error variables)
      pure (variableId $> Type.Error)


checkFunction :: Syntax.Identifier () -> [Type] -> Checker (Syntax.Identifier Type)
checkFunction targetId parameterTypes = go =<< Checker.getFunctions
  where
    key = Unicode.collate targetId.name

    go [] = do
      Checker.updateFunctions \case
        [] -> [Map.singleton key [(parameterTypes, Type.Error)]]
        (head : tail) -> Map.insert key [(parameterTypes, Type.Error)] head : tail

      Checker.report (Error.UnknownFunction targetId parameterTypes)
      pure (targetId $> Type.Function parameterTypes Type.Error)

    go (head : tail) =
      case find (liftEq Type.areCompatible parameterTypes . fst) =<< Map.lookup key head of
        Just _ | Type.Error `elem` parameterTypes ->
          pure (targetId $> Type.Function parameterTypes Type.Error)

        Just (_, returnType) ->
          pure (targetId $> Type.Function parameterTypes returnType)

        Nothing -> go tail
