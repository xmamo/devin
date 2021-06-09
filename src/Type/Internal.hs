module Type.Internal (
  checkDeclarations,
  checkDeclaration,
  checkStatement,
  checkExpression
) where

import Control.Monad
import Data.Either
import Data.Foldable

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
  (_, Syntax.FunctionDeclaration {functionId, parameters, returnInfo, body}) -> do
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

    (returnType, Environment types'' variables'' functions'') <- case returnInfo of
      Just (_, returnTypeId) -> lookupType environment' returnTypeId
      _ -> pure (Unit, environment')

    case pass of
      Pass1 -> do
        let key = Unicode.collate functionName

        if any (\(k, pts, _) -> k == key && pts == parameterTypes) (head functions'') then do
          tell [DuplicateFunctionDefinition functionId parameterTypes]
          pure (Environment types'' variables functions'')
        else
          pure (Environment types'' variables (((key, parameterTypes, returnType) : head functions'') : tail functions''))

      Pass2 -> do
        (doesReturn, _) <- checkStatement returnType (Environment types'' variables'' functions) body
        when (returnType /= Unit && not doesReturn) (tell [MissingReturnPathError functionId parameterTypes])
        pure (Environment types'' variables functions)

  (Pass2, Syntax.VariableDeclaration {variableId, typeInfo = Just (_, typeId), value}) -> do
    let Syntax.Identifier _ variableName = variableId
    (t, environment') <- lookupType environment typeId
    (valueType, Environment types'' variables'' functions'') <- checkExpression environment' value
    when (valueType /= t) (tell [InvalidTypeError value t valueType])
    pure (Environment types'' ((Unicode.collate variableName, t) : variables'') functions'')

  (Pass2, Syntax.VariableDeclaration {variableId, typeInfo = Nothing, value}) -> do
    let Syntax.Identifier _ variableName = variableId
    (valueType, Environment types' variables' functions') <- checkExpression environment value
    pure (Environment types' ((Unicode.collate variableName, valueType) : variables') functions')

  _ -> pure environment


checkStatement :: Type -> Environment -> Syntax.Statement () -> Writer [Error] (Bool, Environment)
checkStatement expectedType environment statement = case statement of
  Syntax.ExpressionStatement {value} -> do
    (_, environment') <- checkExpression environment value
    pure (False, environment')

  Syntax.IfStatement {predicate, trueBranch} -> do
    (predicateType, environment') <- checkExpression environment predicate
    when (predicateType /= Boolean) (tell [InvalidTypeError predicate Boolean predicateType])
    checkStatement expectedType environment' trueBranch
    pure (False, environment')

  Syntax.IfElseStatement {predicate, trueBranch, falseBranch} -> do
    (predicateType, environment') <- checkExpression environment predicate
    when (predicateType /= Boolean) (tell [InvalidTypeError predicate Boolean predicateType])
    (doesTrueBranchReturn, _) <- checkStatement expectedType environment' trueBranch
    (doesFalseBrachReturn, _) <- checkStatement expectedType environment' falseBranch
    pure (doesTrueBranchReturn && doesFalseBrachReturn, environment')

  Syntax.WhileStatement {predicate, body} -> do
    (predicateType, environment') <- checkExpression environment predicate
    when (predicateType /= Boolean) (tell [InvalidTypeError predicate Boolean predicateType])
    checkStatement expectedType environment' body
    pure (False, environment')

  Syntax.DoWhileStatement {body, predicate} -> do
    (doesReturn, _) <- checkStatement expectedType environment body
    (predicateType, environment') <- checkExpression environment predicate
    when (predicateType /= Boolean) (tell [InvalidTypeError predicate Boolean predicateType])
    pure (doesReturn, environment')

  Syntax.ReturnStatement {result = Just result} -> do
    (resultType, environment') <- checkExpression environment result
    when (resultType /= expectedType) (tell [InvalidReturnTypeError statement expectedType resultType])
    pure (True, environment')

  Syntax.ReturnStatement {result = Nothing} -> do
    when (expectedType /= Unit) (tell [MissingReturnValueError statement expectedType])
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
  Syntax.LiteralExpression {literal = Syntax.IntegerLiteral {}} -> pure (Integer, environment)

  Syntax.LiteralExpression {literal = Syntax.RationalLiteral {}} -> pure (Rational, environment)

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
      (Syntax.PlusOperator _, Integer) -> pure (Integer, environment')
      (Syntax.PlusOperator _, Rational) -> pure (Rational, environment')
      (Syntax.MinusOperator _, Integer) -> pure (Integer, environment')
      (Syntax.MinusOperator _, Rational) -> pure (Rational, environment')
      (Syntax.NotOperator _, Boolean) -> pure (Boolean, environment')

      _ -> do
        tell [InvalidUnaryError unary operandType]
        pure (Error, environment')

  Syntax.BinaryExpression {left, binary, right} -> do
    (leftType, environment') <- checkExpression environment left
    (rightType, environment'') <- checkExpression environment' right

    case (leftType, binary, rightType) of
      (Error, _, _) -> pure (Error, environment'')
      (_, _, Error) -> pure (Error, environment'')
      (Integer, Syntax.AddOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.AddOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.SubtractOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.SubtractOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.MultiplyOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.MultiplyOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.DivideOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.DivideOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.RemainderOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.RemainderOperator _, Rational) -> pure (Rational, environment'')
      (_, Syntax.EqualOperator _, _) -> pure (Boolean, environment'')
      (_, Syntax.NotEqualOperator _, _) -> pure (Boolean, environment'')
      (Integer, Syntax.LessOperator _, Integer) -> pure (Boolean, environment'')
      (Rational, Syntax.LessOperator _, Rational) -> pure (Boolean, environment'')
      (Integer, Syntax.LessOrEqualOperator _, Integer) -> pure (Boolean, environment'')
      (Rational, Syntax.LessOrEqualOperator _, Rational) -> pure (Boolean, environment'')
      (Integer, Syntax.GreaterOperator _, Integer) -> pure (Boolean, environment'')
      (Rational, Syntax.GreaterOperator _, Rational) -> pure (Boolean, environment'')
      (Integer, Syntax.GreaterOrEqualOperator _, Integer) -> pure (Boolean, environment'')
      (Rational, Syntax.GreaterOrEqualOperator _, Rational) -> pure (Boolean, environment'')
      (Boolean, Syntax.AndOperator _, Boolean) -> pure (Boolean, environment'')
      (Boolean, Syntax.OrOperator _, Boolean) -> pure (Boolean, environment'')

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
      (Boolean, Syntax.AssignOperator _, Boolean) -> pure (Boolean, environment'')
      (Integer, Syntax.AssignOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.AssignOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.AddAssignOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.AddAssignOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.SubtractAssignOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.SubtractAssignOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.MultiplyAssignOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.MultiplyAssignOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.DivideAssignOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.DivideAssignOperator _, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.RemainderAssignOperator _, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.RemainderAssignOperator _, Rational) -> pure (Rational, environment'')

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

    go functions = case find (\(k, pts, _) -> k == key && pts == parameterTypes) (head functions) of
      Just (_, _, returnType) | not (any isError parameterTypes) -> pure (returnType, environment)
      Just _ -> pure (Error, environment)
      Nothing -> go (tail functions)
