module Checkers (
  checkDeclarations,
  checkStatement,
  checkVariable
) where

import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty
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
      Just (id, colon, typeId, rest) -> for ((undefined, id, colon, typeId) : rest) \(_, _, _, typeId) -> do
        typeId' <- checkType typeId
        pure typeId'.t

      Nothing -> pure []

    returnType <- case returnInfo of
      Just (_, typeId) -> do
        typeId' <- checkType typeId
        pure (typeId'.t)

      Nothing -> pure Type.Unit

    functions <- Checker.getFunctions
    let key = Unicode.collate functionId.name

    case Map.lookup key (NonEmpty.head functions) of
      Just signatures | any (liftEq Type.areCompatible parameterTypes . fst) signatures ->
        Checker.report (Error.FunctionRedefinition functionId parameterTypes)

      Just signatures -> Checker.updateFunctions \functions ->
        Map.insert key ((parameterTypes, returnType) : signatures) (NonEmpty.head functions) :| NonEmpty.tail functions

      Nothing -> Checker.updateFunctions \functions ->
        Map.insert key [(parameterTypes, returnType)] (NonEmpty.head functions) :| NonEmpty.tail functions


checkDeclaration2 :: Syntax.Declaration () -> Checker (Syntax.Declaration Type)
checkDeclaration2 = \case
  Syntax.VariableDeclaration{varKeyword, variableId, typeInfo = Just (colon, typeId), equalSign, value, semicolon} -> do
    typeId' <- checkType typeId
    value' <- checkExpression value
    unless (Type.areCompatible value'.t typeId'.t) (Checker.report (Error.InvalidType value typeId'.t value'.t))
    Checker.updateVariables (Map.insert (Unicode.collate variableId.name) typeId'.t)
    let variableId' = Syntax.Identifier variableId.s variableId.name typeId'.t
    pure (Syntax.VariableDeclaration varKeyword variableId' (Just (colon, typeId')) equalSign value' semicolon)

  Syntax.VariableDeclaration{varKeyword, variableId, typeInfo = Nothing, equalSign, value, semicolon} -> do
    value' <- checkExpression value
    Checker.updateVariables (Map.insert (Unicode.collate variableId.name) value'.t)
    let variableId' = Syntax.Identifier variableId.s variableId.name value'.t
    pure (Syntax.VariableDeclaration varKeyword variableId' Nothing equalSign value' semicolon)

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

    let functionId' = Syntax.Identifier functionId.s functionId.name (Type.Function [local.t | local <- locals] returnType)

    body' <- Checker.scoped do
      let f variables local = Map.insert (Unicode.collate local.name) local.t variables
      Checker.updateVariables \variables -> foldl' f variables locals
      Checker.updateFunctions (Map.empty <|)
      checkStatement returnType body

    unless (Syntax.doesReturn body' || Type.areCompatible returnType Type.Unit) $
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
    let predicateOk = Type.areCompatible predicate'.t Type.Boolean
    unless predicateOk (Checker.report (Error.InvalidType predicate Type.Boolean predicate'.t))
    statement' <- Checker.scoped (checkStatement expectedType trueBranch)
    pure (Syntax.IfStatement ifKeyword predicate' statement')

  Syntax.IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Boolean
    unless predicateOk (Checker.report (Error.InvalidType predicate Type.Boolean predicate'.t))
    trueBranch' <- Checker.scoped (checkStatement expectedType trueBranch)
    falseBranch' <- Checker.scoped (checkStatement expectedType falseBranch)
    pure (Syntax.IfElseStatement ifKeyword predicate' trueBranch' elseKeyword falseBranch')

  Syntax.WhileStatement{whileKeyword, predicate, body} -> do
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Boolean
    unless predicateOk (Checker.report (Error.InvalidType predicate Type.Boolean predicate'.t))
    statement' <- Checker.scoped (checkStatement expectedType body)
    pure (Syntax.WhileStatement whileKeyword predicate' statement')

  Syntax.DoWhileStatement{doKeyword, body, whileKeyword, predicate, semicolon} -> do
    statement' <- checkStatement expectedType body
    predicate' <- checkExpression predicate
    let predicateOk = Type.areCompatible predicate'.t Type.Boolean
    unless predicateOk (Checker.report (Error.InvalidType predicate Type.Boolean predicate'.t))
    pure (Syntax.DoWhileStatement doKeyword statement' whileKeyword predicate' semicolon)

  Syntax.ReturnStatement{returnKeyword, result = Just result, semicolon} -> do
    result' <- checkExpression result
    let resultOk = Type.areCompatible result'.t expectedType
    unless resultOk (Checker.report (Error.InvalidReturnType statement expectedType result'.t))
    pure (Syntax.ReturnStatement returnKeyword (Just result') semicolon)

  Syntax.ReturnStatement{returnKeyword, result = Nothing, semicolon} -> do
    unless (Type.areCompatible expectedType Type.Unit) (Checker.report (Error.MissingReturnValue statement expectedType))
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
  Syntax.IntegerExpression{} -> pure (expression $> Type.Integer)

  Syntax.RationalExpression{} -> pure (expression $> Type.Rational)

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
      (Syntax.PlusOperator{}, Type.Integer) -> pure Type.Integer
      (Syntax.PlusOperator{}, Type.Rational) -> pure Type.Rational
      (Syntax.MinusOperator{}, Type.Integer) -> pure Type.Integer
      (Syntax.MinusOperator{}, Type.Rational) -> pure Type.Rational
      (Syntax.NotOperator{}, Type.Boolean) -> pure Type.Boolean
      _ -> Checker.report (Error.InvalidUnary unary operand'.t) $> Type.Error

    pure (Syntax.UnaryExpression (unary $> Type.Function [operand'.t] t) operand' t)

  Syntax.BinaryExpression{left, binary, right} -> do
    left' <- checkExpression left
    right' <- checkExpression right

    t <- case (left'.t, binary, right'.t) of
      (Type.Error, _, _) -> pure Type.Error
      (_, _, Type.Error) -> pure Type.Error
      (Type.Integer, Syntax.AddOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.AddOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.SubtractOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.SubtractOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.MultiplyOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.MultiplyOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.DivideOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.DivideOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.RemainderOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.RemainderOperator{}, Type.Rational) -> pure Type.Rational
      (_, Syntax.EqualOperator{}, _) -> pure Type.Boolean
      (_, Syntax.NotEqualOperator{}, _) -> pure Type.Boolean
      (Type.Integer, Syntax.LessOperator{}, Type.Integer) -> pure Type.Boolean
      (Type.Rational, Syntax.LessOperator{}, Type.Rational) -> pure Type.Boolean
      (Type.Integer, Syntax.LessOrEqualOperator{}, Type.Integer) -> pure Type.Boolean
      (Type.Rational, Syntax.LessOrEqualOperator{}, Type.Rational) -> pure Type.Boolean
      (Type.Integer, Syntax.GreaterOperator{}, Type.Integer) -> pure Type.Boolean
      (Type.Rational, Syntax.GreaterOperator{}, Type.Rational) -> pure Type.Boolean
      (Type.Integer, Syntax.GreaterOrEqualOperator{}, Type.Integer) -> pure Type.Boolean
      (Type.Rational, Syntax.GreaterOrEqualOperator{}, Type.Rational) -> pure Type.Boolean
      (Type.Boolean, Syntax.AndOperator{}, Type.Boolean) -> pure Type.Boolean
      (Type.Boolean, Syntax.OrOperator{}, Type.Boolean) -> pure Type.Boolean
      _ -> Checker.report (Error.InvalidBinary binary left'.t right'.t) $> Type.Error

    pure (Syntax.BinaryExpression left' (binary $> Type.Function [left'.t, right'.t] t) right' t)

  Syntax.AssignExpression{targetId, assign, value} -> do
    targetId' <- checkVariable targetId
    value' <- checkExpression value

    t <- case (targetId'.t, assign, value'.t) of
      (Type.Error, _, _) -> pure Type.Error
      (_, _, Type.Error) -> pure Type.Error
      (Type.Unit, Syntax.AssignOperator{}, Type.Unit) -> pure Type.Unit
      (Type.Boolean, Syntax.AssignOperator{}, Type.Boolean) -> pure Type.Boolean
      (Type.Integer, Syntax.AssignOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.AssignOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.AddAssignOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.AddAssignOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.SubtractAssignOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.SubtractAssignOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.MultiplyAssignOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.MultiplyAssignOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.DivideAssignOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.DivideAssignOperator{}, Type.Rational) -> pure Type.Rational
      (Type.Integer, Syntax.RemainderAssignOperator{}, Type.Integer) -> pure Type.Integer
      (Type.Rational, Syntax.RemainderAssignOperator{}, Type.Rational) -> pure Type.Rational
      _ -> Checker.report (Error.InvalidAssign assign targetId'.t value'.t) $> Type.Error

    pure (Syntax.AssignExpression targetId' (assign $> Type.Function [targetId'.t, value'.t] t) value' value'.t)

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
      Checker.updateTypes (Map.insert key (Type.Unknown name))
      pure (Syntax.Identifier s name (Type.Unknown name))


checkVariable :: Syntax.Identifier () -> Checker (Syntax.Identifier Type)
checkVariable variableId = do
  variables <- Checker.getVariables
  let key = Unicode.collate variableId.name

  case Map.lookup key variables of
    Just t -> pure (variableId $> t)

    Nothing -> do
      Checker.report (Error.UnknownVariable variableId)
      Checker.updateVariables (Map.insert key Type.Error)
      pure (variableId $> Type.Error)


checkFunction :: Syntax.Identifier () -> [Type] -> Checker (Syntax.Identifier Type)
checkFunction targetId parameterTypes = go =<< Checker.getFunctions
  where
    key = Unicode.collate targetId.name

    go functions = do
      let (head, tail) = NonEmpty.uncons functions

      case (find (liftEq Type.areCompatible parameterTypes . fst) =<< Map.lookup key head, tail) of
        (Just _, _) | Type.Error `elem` parameterTypes -> pure (targetId $> Type.Function parameterTypes Type.Error)

        (Just (_, returnType), _) -> pure (targetId $> Type.Function parameterTypes returnType)

        (Nothing, Just tail) -> go tail

        (Nothing, Nothing) -> do
          Checker.report (Error.UnknownFunction targetId parameterTypes)
          Checker.updateFunctions (Map.singleton key [(parameterTypes, Type.Error)] <|)
          pure (targetId $> Type.Function parameterTypes Type.Error)
