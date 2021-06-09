module Type.Internal (
  checkDeclarations,
  checkDeclaration,
  checkStatement,
  checkExpression
) where

import Control.Monad
import Data.Either
import Data.Foldable
import Data.Functor.Classes

import Control.Monad.Trans.Writer

import qualified Syntax
import Type.Common
import qualified Unicode


checkDeclarations :: Foldable t => Environment -> t (Syntax.Declaration ()) -> Writer [Error] Environment
checkDeclarations environment declarations = do
  environment' <- foldlM (checkDeclaration Pass1) environment declarations
  foldlM (checkDeclaration Pass2) environment' declarations


checkDeclaration :: Pass -> Environment -> Syntax.Declaration () -> Writer [Error] Environment
checkDeclaration pass environment declaration = case (pass, declaration) of
  (_, Syntax.FunctionDeclaration {functionId, parameters, result, body}) -> do
    let Environment _ variables functions = environment
        Syntax.Identifier _ functionName = functionId

    (parameterTypes, environment') <- case parameters of
      Just ((id, _, typeId), rest) ->
        foldlM f ([], environment) ((id, typeId) : [(id, typeId) | (_, id, _, typeId) <- rest])
        where
          f (parameterTypes, environment) (Syntax.Identifier _ name, typeId) = do
            (t, Environment types' variables' functions') <- lookupType environment typeId
            pure (parameterTypes ++ [t], Environment types' ((Unicode.collate name, t) : variables') functions')

      Nothing -> pure ([], environment)

    (returnType, Environment types'' variables'' functions'') <- case result of
      Just (_, returnTypeId) -> lookupType environment' returnTypeId
      _ -> pure (Unit, environment')

    case pass of
      Pass1 -> do
        let key = Unicode.collate functionName

        if any (\(k, pts, _) -> k == key && liftEq isCompatible pts parameterTypes) (head functions'') then do
          tell [DuplicateFunctionDefinition functionId parameterTypes]
          pure (Environment types'' variables functions'')
        else
          pure (Environment types'' variables (((key, parameterTypes, returnType) : head functions'') : tail functions''))

      Pass2 -> do
        (doesReturn, _) <- checkStatement returnType (Environment types'' variables'' functions) body
        unless (isCompatible returnType Unit || doesReturn) (tell [MissingReturnPathError functionId parameterTypes])
        pure (Environment types'' variables functions)

  (Pass2, Syntax.VariableDeclaration {variableId = Syntax.Identifier _ variableName, typeId, value}) -> do
    (t, environment') <- lookupType environment typeId
    (valueType, Environment types'' variables'' functions'') <- checkExpression environment' value
    unless (isCompatible valueType t) (tell [InvalidTypeError value t valueType])
    pure (Environment types'' ((Unicode.collate variableName, t) : variables'') functions'')

  _ -> pure environment


checkStatement :: Type -> Environment -> Syntax.Statement () -> Writer [Error] (Bool, Environment)
checkStatement expectedType environment statement = case statement of
  Syntax.ExpressionStatement {value} -> do
    (_, environment') <- checkExpression environment value
    pure (False, environment')

  Syntax.IfStatement {predicate, trueBranch} -> do
    (predicateType, environment') <- checkExpression environment predicate
    unless (isCompatible predicateType Bool) (tell [InvalidTypeError predicate Bool predicateType])
    checkStatement expectedType environment' trueBranch
    pure (False, environment')

  Syntax.IfElseStatement {predicate, trueBranch, falseBranch} -> do
    (predicateType, environment') <- checkExpression environment predicate
    unless (isCompatible predicateType Bool) (tell [InvalidTypeError predicate Bool predicateType])
    (doesTrueBranchReturn, _) <- checkStatement expectedType environment' trueBranch
    (doesFalseBrachReturn, _) <- checkStatement expectedType environment' falseBranch
    pure (doesTrueBranchReturn && doesFalseBrachReturn, environment')

  Syntax.WhileStatement {predicate, body} -> do
    (predicateType, environment') <- checkExpression environment predicate
    unless (isCompatible predicateType Bool) (tell [InvalidTypeError predicate Bool predicateType])
    checkStatement expectedType environment' body
    pure (False, environment')

  Syntax.DoWhileStatement {body, predicate} -> do
    (doesReturn, _) <- checkStatement expectedType environment body
    (predicateType, environment') <- checkExpression environment predicate
    unless (isCompatible predicateType Bool) (tell [InvalidTypeError predicate Bool predicateType])
    pure (doesReturn, environment')

  Syntax.ReturnStatement {result = Just result} -> do
    (resultType, environment') <- checkExpression environment result
    unless (isCompatible resultType expectedType) (tell [InvalidReturnTypeError statement expectedType resultType])
    pure (True, environment')

  Syntax.ReturnStatement {result = Nothing} -> do
    unless (isCompatible expectedType Unit) (tell [MissingReturnValueError statement expectedType])
    pure (True, environment)

  Syntax.BlockStatement {elements} -> do
    let Environment types variables functions = environment
    environment' <- foldlM (checkDeclaration Pass1) (Environment types variables ([] : functions)) (lefts elements)
    (doesReturn, _) <- foldlM f (False, environment') elements
    pure (doesReturn, environment)
    where
      f (doesReturn, environment) (Left declaration) = do
        environment' <- checkDeclaration Pass2 environment declaration
        pure (doesReturn, environment')

      f (doesReturn, environment) (Right statement) = do
        (doesReturn', environment') <- checkStatement expectedType environment statement
        pure (doesReturn || doesReturn', environment')


checkExpression :: Environment -> Syntax.Expression () -> Writer [Error] (Type, Environment)
checkExpression environment = \case
  Syntax.IntegerExpression {} -> pure (Int, environment)

  Syntax.VariableExpression {variableId} -> lookupVariableType environment variableId

  Syntax.CallExpression {targetId, arguments} -> do
    (argumentTs, environment') <- case arguments of
      Just (first, rest) -> foldlM f ([], environment) (first : map snd rest)
        where
          f (argumentTs, environment) argument = do
            (t, environment') <- checkExpression environment argument
            pure (argumentTs ++ [t], environment')

      Nothing -> pure ([], environment)

    lookupFunctionType environment' targetId argumentTs

  Syntax.UnaryExpression {unary, operand} -> do
    (operandType, environment') <- checkExpression environment operand

    case (unary, operandType) of
      (_, Error) -> pure (Error, environment')
      (Syntax.PlusOperator _, Int) -> pure (Int, environment')
      (Syntax.PlusOperator _, Float) -> pure (Float, environment')
      (Syntax.MinusOperator _, Int) -> pure (Int, environment')
      (Syntax.MinusOperator _, Float) -> pure (Float, environment')
      (Syntax.NotOperator _, Bool) -> pure (Bool, environment')

      _ -> do
        tell [InvalidUnaryError unary operandType]
        pure (Error, environment')

  Syntax.BinaryExpression {left, binary, right} -> do
    (leftType, environment') <- checkExpression environment left
    (rightType, environment'') <- checkExpression environment' right

    case (leftType, binary, rightType) of
      (Error, _, _) -> pure (Error, environment'')
      (_, _, Error) -> pure (Error, environment'')
      (Int, Syntax.AddOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.AddOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.SubtractOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.SubtractOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.MultiplyOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.MultiplyOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.DivideOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.DivideOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.RemainderOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.RemainderOperator _, Float) -> pure (Float, environment'')
      (_, Syntax.EqualOperator _, _) -> pure (Bool, environment'')
      (_, Syntax.NotEqualOperator _, _) -> pure (Bool, environment'')
      (Int, Syntax.LessOperator _, Int) -> pure (Bool, environment'')
      (Float, Syntax.LessOperator _, Float) -> pure (Bool, environment'')
      (Int, Syntax.LessOrEqualOperator _, Int) -> pure (Bool, environment'')
      (Float, Syntax.LessOrEqualOperator _, Float) -> pure (Bool, environment'')
      (Int, Syntax.GreaterOperator _, Int) -> pure (Bool, environment'')
      (Float, Syntax.GreaterOperator _, Float) -> pure (Bool, environment'')
      (Int, Syntax.GreaterOrEqualOperator _, Int) -> pure (Bool, environment'')
      (Float, Syntax.GreaterOrEqualOperator _, Float) -> pure (Bool, environment'')
      (Bool, Syntax.AndOperator _, Bool) -> pure (Bool, environment'')
      (Bool, Syntax.OrOperator _, Bool) -> pure (Bool, environment'')

      _ -> do
        tell [InvalidBinaryError binary leftType rightType]
        pure (Error, environment'')

  Syntax.AssignExpression {targetId, assign, value} -> do
    (valueType, environment') <- checkExpression environment value
    (targetType, environment'') <- lookupVariableType environment' targetId

    case (targetType, assign, valueType) of
      (Error, _, _) -> pure (Error, environment'')
      (_, _, Error) -> pure (Error, environment'')
      (Unit, Syntax.AssignOperator _, Unit) -> pure (Unit, environment'')
      (Bool, Syntax.AssignOperator _, Bool) -> pure (Bool, environment'')
      (Int, Syntax.AssignOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.AssignOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.AddAssignOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.AddAssignOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.SubtractAssignOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.SubtractAssignOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.MultiplyAssignOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.MultiplyAssignOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.DivideAssignOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.DivideAssignOperator _, Float) -> pure (Float, environment'')
      (Int, Syntax.RemainderAssignOperator _, Int) -> pure (Int, environment'')
      (Float, Syntax.RemainderAssignOperator _, Float) -> pure (Float, environment'')

      _ -> do
        tell [InvalidAssignError assign targetType valueType]
        pure (Error, environment'')

  Syntax.ParenthesizedExpression {inner} -> checkExpression environment inner


lookupType :: Environment -> Syntax.Identifier -> Writer [Error] (Type, Environment)
lookupType environment typeId = do
  let Environment types variables functions = environment
      Syntax.Identifier _ typeName = typeId
      key = Unicode.collate typeName

  case lookup key types of
    Just t -> pure (t, environment)

    Nothing -> do
      tell [UnknownTypeError typeId]
      pure (Unknown typeName, Environment ((key, Error) : types) variables functions)


lookupVariableType :: Environment -> Syntax.Identifier -> Writer [Error] (Type, Environment)
lookupVariableType environment variableId = do
  let Environment types variables functions = environment
      Syntax.Identifier _ variableName = variableId
      key = Unicode.collate variableName
  
  case lookup key variables of
    Just t -> pure (t, environment)

    Nothing -> do
      tell [UnknownIdentifierError variableId]
      pure (Error, Environment types ((key, Error) : variables) functions)


lookupFunctionType :: Environment -> Syntax.Identifier -> [Type] -> Writer [Error] (Type, Environment)
lookupFunctionType environment functionId parameterTypes = go functions
  where
    Environment types variables functions = environment
    Syntax.Identifier _ functionName = functionId
    key = Unicode.collate functionName

    go [] = do
      tell [UnknownFunctionError functionId parameterTypes]
      pure (Error, Environment types variables (((key, parameterTypes, Error) : head functions) : tail functions))

    go functions = case find (\(k, pts, _) -> k == key && liftEq isCompatible pts parameterTypes) (head functions) of
      Just (_, _, returnType) | Error `notElem` parameterTypes -> pure (returnType, environment)
      Just _ -> pure (Error, environment)
      Nothing -> go (tail functions)


isCompatible :: Type -> Type -> Bool
isCompatible Error _ = True
isCompatible _ Error = True
isCompatible t1 t2 = t1 == t2
