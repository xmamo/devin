module Type.Internal (
  checkDeclarations,
  checkDeclaration1,
  checkDeclaration2,
  checkStatement,
  checkExpression,
  checkType,
  checkVariable,
  checkFunction
) where

import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Map as Map

import Control.Monad.Trans.Writer

import qualified Syntax
import qualified Unicode

import Type.Common


checkDeclarations :: Environment -> [Syntax.Declaration ()] -> Writer [Error] ([Syntax.Declaration Type], Environment)
checkDeclarations environment declarations = do
  environment' <- foldlM checkDeclaration1 environment declarations
  foldlM g ([], environment') declarations
  where
    g (declarations, environment) declaration = do
      (declaration', environment') <- checkDeclaration2 environment declaration
      pure (declarations ++ [declaration'], environment')


checkDeclaration1 :: Environment -> Syntax.Declaration () -> Writer [Error] Environment
checkDeclaration1 environment = \case
  Syntax.VariableDeclaration{} -> pure environment

  Syntax.FunctionDeclaration{functionId, parameters, returnInfo} -> do
    (parameterTypes, environment') <- case parameters of
      Just (id, colon, typeId, rest) -> foldlM f ([], environment) ((undefined, id, colon, typeId) : rest)
        where
          f (types, environment) (_, _, _, typeId) = do
            (typeId', environment') <- checkType environment typeId
            pure (types ++ [typeId'.t], environment')

      Nothing -> pure ([], environment)

    (returnType, environment''@Environment{functions = functions''}) <- case returnInfo of
      Just (_, typeId) -> do
        (typeId', environment'') <- checkType environment' typeId
        pure (typeId'.t, environment'')

      Nothing -> pure (Unit, environment')

    let key = Unicode.collate functionId.name

        f (Just signatures) | any (liftEq isCompatible parameterTypes . fst) signatures = do
          tell [FunctionRedefinitionError functionId parameterTypes]
          pure (Just signatures)

        f (Just signatures) = pure (Just ((parameterTypes, returnType) : signatures))

        f Nothing = pure (Just [(parameterTypes, returnType)])

    functions''' <- Map.alterF f key (NonEmpty.head functions'') <&> (:| NonEmpty.tail functions'')
    pure environment''{functions = functions'''}


checkDeclaration2 :: Environment -> Syntax.Declaration () -> Writer [Error] (Syntax.Declaration Type, Environment)
checkDeclaration2 environment = \case
  Syntax.VariableDeclaration{varKeyword, variableId, typeInfo = Just (colon, typeId), equalSign, value, semicolon} -> do
    (typeId', environment') <- checkType environment typeId
    let typeInfo' = Just (colon, typeId')
    (value', environment''@Environment{variables = variables''}) <- checkExpression environment' value
    unless (value'.t `isCompatible` typeId'.t) (tell [InvalidTypeError value typeId'.t value'.t])
    let environment''' = environment''{variables = Map.insert (Unicode.collate variableId.name) typeId'.t variables''}
    let variableId' = Syntax.Identifier variableId.s variableId.name typeId'.t
    pure (Syntax.VariableDeclaration varKeyword variableId' typeInfo' equalSign value' semicolon, environment''')

  Syntax.VariableDeclaration{varKeyword, variableId, typeInfo = Nothing, equalSign, value, semicolon} -> do
    (value', environment'@Environment{variables = variables'}) <- checkExpression environment value
    let environment'' = environment'{variables = Map.insert (Unicode.collate variableId.name) value'.t variables'}
    let variableId' = Syntax.Identifier variableId.s variableId.name value'.t
    pure (Syntax.VariableDeclaration varKeyword variableId' Nothing equalSign value' semicolon, environment'')

  Syntax.FunctionDeclaration{defKeyword, functionId, open, parameters, close, returnInfo, body} -> do
    (locals, parameters', environment') <- case parameters of
      Just (Syntax.Identifier{s, name}, colon, typeId, rest) -> do
        (typeId', environment') <- checkType environment typeId
        let id' = Syntax.Identifier s name typeId'.t
        (locals, rest', environment'') <- foldlM f (Map.singleton (Unicode.collate name) id'.t, [], environment') rest
        pure (locals, Just (id', colon, typeId', rest'), environment'')
        where
          f (locals, rest, environment) (comma, Syntax.Identifier s name _, colon, typeId) = do
            (typeId', environment') <- checkType environment typeId
            let id' = Syntax.Identifier s name typeId'.t
            let locals' = Map.insert (Unicode.collate name) typeId'.t locals
            pure (locals', rest ++ [(comma, id', colon, typeId')], environment')

      Nothing -> pure (Map.empty, Nothing, environment)

    (returnType, returnInfo', environment''@Environment{variables = variables''}) <- case returnInfo of
      Just (arrow, typeId) -> do
        (typeId', environment'') <- checkType environment' typeId
        pure (typeId'.t, Just (arrow, typeId'), environment'')

      Nothing -> pure (Unit, Nothing, environment')

    let functionId' = Syntax.Identifier functionId.s functionId.name (Function (Map.elems locals) returnType)
    (body', _) <- checkStatement returnType (environment''{variables = Map.union locals variables''}) body
    pure (Syntax.FunctionDeclaration defKeyword functionId' open parameters' close returnInfo' body', environment'')


checkStatement :: Type -> Environment -> Syntax.Statement () -> Writer [Error] (Syntax.Statement Type, Environment)
checkStatement expectedType environment@Environment{functions} statement = case statement of
  Syntax.ExpressionStatement{value, semicolon} -> do
    (value', environment') <- checkExpression environment value
    unless (Syntax.hasSideEffects value) (tell [NoSideEffectsError statement])
    pure (Syntax.ExpressionStatement value' semicolon, environment')

  Syntax.IfStatement{ifKeyword, predicate, trueBranch} -> do
    (predicate', environment') <- checkExpression environment predicate
    unless (predicate'.t `isCompatible` Boolean) (tell [InvalidTypeError predicate Boolean predicate'.t])
    (statement', _) <- checkStatement expectedType environment' trueBranch
    pure (Syntax.IfStatement ifKeyword predicate' statement', environment')

  Syntax.IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    (predicate', environment') <- checkExpression environment predicate
    unless (predicate'.t `isCompatible` Boolean) (tell [InvalidTypeError predicate Boolean predicate'.t])
    (trueBranch', _) <- checkStatement expectedType environment' trueBranch
    (falseBranch', _) <- checkStatement expectedType environment' falseBranch
    pure (Syntax.IfElseStatement ifKeyword predicate' trueBranch' elseKeyword falseBranch', environment')

  Syntax.WhileStatement{whileKeyword, predicate, body} -> do
    (predicate', environment') <- checkExpression environment predicate
    unless (predicate'.t `isCompatible` Boolean) (tell [InvalidTypeError predicate Boolean predicate'.t])
    (statement', _) <- checkStatement expectedType environment' body
    pure (Syntax.WhileStatement whileKeyword predicate' statement', environment')

  Syntax.DoWhileStatement{doKeyword, body, whileKeyword, predicate, semicolon} -> do
    (statement', _) <- checkStatement expectedType environment body
    (predicate', environment') <- checkExpression environment predicate
    unless (predicate'.t `isCompatible` Boolean) (tell [InvalidTypeError predicate Boolean predicate'.t])
    pure (Syntax.DoWhileStatement doKeyword statement' whileKeyword predicate' semicolon, environment')

  Syntax.ReturnStatement{returnKeyword, result = Just result, semicolon} -> do
    (result', environment') <- checkExpression environment result
    unless (result'.t `isCompatible` expectedType) (tell [InvalidReturnTypeError statement expectedType result'.t])
    pure (Syntax.ReturnStatement returnKeyword (Just result') semicolon, environment')

  Syntax.ReturnStatement{returnKeyword, result = Nothing, semicolon} -> do
    unless (expectedType `isCompatible` Unit) (tell [MissingReturnValueError statement expectedType])
    pure (Syntax.ReturnStatement returnKeyword Nothing semicolon, environment)

  Syntax.BlockStatement{open, elements, close} -> do
    environment' <- foldlM f environment{functions = Map.empty <| functions} elements
    (elements'', _) <- foldlM g ([], environment') elements
    pure (Syntax.BlockStatement open elements'' close, environment)
    where
      f environment = either (checkDeclaration1 environment) (const (pure environment))

      g (elements, environment) (Left declaration) = do
        (declaration', environment') <- checkDeclaration2 environment declaration
        pure (elements ++ [Left declaration'], environment')

      g (elements, environment) (Right statement) = do
        (statement', environment') <- checkStatement expectedType environment statement
        pure (elements ++ [Right statement'], environment')


checkExpression :: Environment -> Syntax.Expression () -> Writer [Error] (Syntax.Expression Type, Environment)
checkExpression environment expression = case expression of
  Syntax.IntegerExpression{} -> pure (expression $> Integer, environment)

  Syntax.RationalExpression{} -> pure (expression $> Rational, environment)

  Syntax.VariableExpression{variableId} -> do
    (variableId', environment') <- checkVariable environment variableId
    pure (Syntax.VariableExpression variableId' variableId'.t, environment')

  Syntax.CallExpression{targetId, open, arguments = Just (first, rest), close} -> do
    (first', environment') <- checkExpression environment first
    (rest', environment'') <- foldlM f ([], environment') rest
    let arguments' = Just (first', rest')
    (targetId', environment''') <- checkFunction environment'' targetId (first'.t : [a.t | (_, a) <- rest'])
    pure (Syntax.CallExpression targetId' open arguments' close targetId'.t.returnType, environment''')
    where
      f (arguments, environment) (comma, argument) = do
        (argument', environment') <- checkExpression environment argument
        pure (arguments ++ [(comma, argument')], environment')

  Syntax.CallExpression{targetId, open, arguments = Nothing, close} -> do
    (targetId', environment') <- checkFunction environment targetId []
    pure (Syntax.CallExpression targetId' open Nothing close targetId'.t.returnType, environment')

  Syntax.UnaryExpression{unary, operand} -> do
    (operand', environment') <- checkExpression environment operand

    t <- case (unary, operand'.t) of
      (_, Error) -> pure Error
      (Syntax.PlusOperator{}, Integer) -> pure Integer
      (Syntax.PlusOperator{}, Rational) -> pure Rational
      (Syntax.MinusOperator{}, Integer) -> pure Integer
      (Syntax.MinusOperator{}, Rational) -> pure Rational
      (Syntax.NotOperator{}, Boolean) -> pure Boolean
      _ -> tell [InvalidUnaryError unary operand'.t] $> Error

    let unary' = unary $> Function [operand'.t] t
    pure (Syntax.UnaryExpression unary' operand' t, environment')

  Syntax.BinaryExpression{left, binary, right} -> do
    (left', environment') <- checkExpression environment left
    (right', environment'') <- checkExpression environment' right

    t <- case (left'.t, binary, right'.t) of
      (Error, _, _) -> pure Error
      (_, _, Error) -> pure Error
      (Integer, Syntax.AddOperator{}, Integer) -> pure Integer
      (Rational, Syntax.AddOperator{}, Rational) -> pure Rational
      (Integer, Syntax.SubtractOperator{}, Integer) -> pure Integer
      (Rational, Syntax.SubtractOperator{}, Rational) -> pure Rational
      (Integer, Syntax.MultiplyOperator{}, Integer) -> pure Integer
      (Rational, Syntax.MultiplyOperator{}, Rational) -> pure Rational
      (Integer, Syntax.DivideOperator{}, Integer) -> pure Integer
      (Rational, Syntax.DivideOperator{}, Rational) -> pure Rational
      (Integer, Syntax.RemainderOperator{}, Integer) -> pure Integer
      (Rational, Syntax.RemainderOperator{}, Rational) -> pure Rational
      (_, Syntax.EqualOperator{}, _) -> pure Boolean
      (_, Syntax.NotEqualOperator{}, _) -> pure Boolean
      (Integer, Syntax.LessOperator{}, Integer) -> pure Boolean
      (Rational, Syntax.LessOperator{}, Rational) -> pure Boolean
      (Integer, Syntax.LessOrEqualOperator{}, Integer) -> pure Boolean
      (Rational, Syntax.LessOrEqualOperator{}, Rational) -> pure Boolean
      (Integer, Syntax.GreaterOperator{}, Integer) -> pure Boolean
      (Rational, Syntax.GreaterOperator{}, Rational) -> pure Boolean
      (Integer, Syntax.GreaterOrEqualOperator{}, Integer) -> pure Boolean
      (Rational, Syntax.GreaterOrEqualOperator{}, Rational) -> pure Boolean
      (Boolean, Syntax.AndOperator{}, Boolean) -> pure Boolean
      (Boolean, Syntax.OrOperator{}, Boolean) -> pure Boolean
      _ -> tell [InvalidBinaryError binary left'.t right'.t] $> Error

    let binary' = binary $> Function [left'.t, right'.t] t
    pure (Syntax.BinaryExpression left' binary' right' t, environment'')

  Syntax.AssignExpression{targetId, assign, value} -> do
    (targetId', environment') <- checkVariable environment targetId
    (value', environment'') <- checkExpression environment' value

    t <- case (targetId'.t, assign, value'.t) of
      (Error, _, _) -> pure Error
      (_, _, Error) -> pure Error
      (Unit, Syntax.AssignOperator{}, Unit) -> pure Unit
      (Boolean, Syntax.AssignOperator{}, Boolean) -> pure Boolean
      (Integer, Syntax.AssignOperator{}, Integer) -> pure Integer
      (Rational, Syntax.AssignOperator{}, Rational) -> pure Rational
      (Integer, Syntax.AddAssignOperator{}, Integer) -> pure Integer
      (Rational, Syntax.AddAssignOperator{}, Rational) -> pure Rational
      (Integer, Syntax.SubtractAssignOperator{}, Integer) -> pure Integer
      (Rational, Syntax.SubtractAssignOperator{}, Rational) -> pure Rational
      (Integer, Syntax.MultiplyAssignOperator{}, Integer) -> pure Integer
      (Rational, Syntax.MultiplyAssignOperator{}, Rational) -> pure Rational
      (Integer, Syntax.DivideAssignOperator{}, Integer) -> pure Integer
      (Rational, Syntax.DivideAssignOperator{}, Rational) -> pure Rational
      (Integer, Syntax.RemainderAssignOperator{}, Integer) -> pure Integer
      (Rational, Syntax.RemainderAssignOperator{}, Rational) -> pure Rational
      _ -> tell [InvalidAssignError assign targetId'.t value'.t] $> Error

    let assign' = assign $> Function [targetId'.t, value'.t] t
    pure (Syntax.AssignExpression targetId' assign' value' value'.t, environment'')

  Syntax.ParenthesizedExpression{open, inner, close} -> do
    (inner', environment') <- checkExpression environment inner
    pure (Syntax.ParenthesizedExpression open inner' close inner'.t, environment')


checkType :: Environment -> Syntax.Identifier () -> Writer [Error] (Syntax.Identifier Type, Environment)
checkType environment@Environment{types} typeId@Syntax.Identifier{s, name} = do
  let key = Unicode.collate typeId.name

  case Map.insertLookupWithKey (\_ _ old -> old) key Error types of
    (Just t, types') -> pure (Syntax.Identifier s name t, environment{types = types'})

    (Nothing, types') -> do
      tell [UnknownTypeError typeId]
      pure (Syntax.Identifier s name (Unknown name), environment{types = types'})


checkVariable :: Environment -> Syntax.Identifier () -> Writer [Error] (Syntax.Identifier Type, Environment)
checkVariable environment@Environment{variables} variableId = do
  let key = Unicode.collate variableId.name

  case Map.insertLookupWithKey (\_ _ old -> old) key Error variables of
    (Just t, variables') -> pure (variableId $> t, environment{variables = variables'})

    (Nothing, _) -> do
      tell [UnknownVariableError variableId]
      pure (variableId $> Error, environment)


checkFunction :: Environment -> Syntax.Identifier () -> [Type] -> Writer [Error] (Syntax.Identifier Type, Environment)
checkFunction environment@Environment{functions} targetId parameterTypes = go functions
  where
    key = Unicode.collate targetId.name

    go functions = do
      let (head, tail) = NonEmpty.uncons functions

      case (find (liftEq isCompatible parameterTypes . fst) =<< Map.lookup key head, tail) of
        (Just _, _) | Error `elem` parameterTypes -> pure (targetId $> Function parameterTypes Error, environment)

        (Just (_, returnType), _) -> pure (targetId $> Function parameterTypes returnType, environment)

        (Nothing, Just tail) -> go tail

        (Nothing, Nothing) -> do
          tell [UnknownFunctionError targetId parameterTypes]
          let functions' = Map.singleton key [(parameterTypes, Error)] :| []
          pure (targetId $> Function parameterTypes Error, environment{functions = functions'})
