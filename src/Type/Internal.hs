module Type.Internal (
  checkDeclarations,
  checkDeclaration,
  checkStatement,
  checkExpression
) where

import Control.Monad
import Data.Either
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Map as Map

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
        Syntax.Identifier _ functionName () = functionId

    (parameterTypes, environment') <- case parameters of
      Just ((id, colon, typeId), rest) -> foldlM f ([], environment) ((undefined, id, colon, typeId) : rest)
        where
          f (parameterTypes, environment) (_, Syntax.Identifier {name}, _, typeId) = do
            (t, Environment types' variables' functions') <- lookupType environment typeId
            let variables'' = Map.insert (Unicode.collate name) t variables'
            pure (parameterTypes ++ [t], Environment types' variables'' functions')

      Nothing -> pure ([], environment)

    (returnType, Environment types'' variables'' functions'') <- case returnInfo of
      Just (_, returnTypeId) -> lookupType environment' returnTypeId
      _ -> pure (Unit, environment')

    case pass of
      Pass1 -> do
        let key = Unicode.collate functionName
        functions''' <- Map.alterF f key (NonEmpty.head functions'') <&> (:| NonEmpty.tail functions'')
        pure (Environment types'' variables functions''')

        where
          f (Just signatures) | any ((== parameterTypes) . fst) signatures = do
            tell [DuplicateFunctionDefinition functionId parameterTypes]
            pure (Just signatures)

          f (Just signatures) = pure (Just ((parameterTypes, returnType) : signatures))

          f Nothing = pure (Just [(parameterTypes, returnType)])

      Pass2 -> do
        (doesReturn, _) <- checkStatement returnType (Environment types'' variables'' functions) body
        when (returnType /= Unit && not doesReturn) (tell [MissingReturnPathError functionId parameterTypes])
        pure (Environment types'' variables functions)

  (Pass2, Syntax.VariableDeclaration {variableId, typeInfo = Just (_, typeId), value}) -> do
    let Syntax.Identifier {name} = variableId
    (t, environment') <- lookupType environment typeId
    (valueType, Environment types'' variables'' functions'') <- checkExpression environment' value
    when (valueType /= t) (tell [InvalidTypeError value t valueType])
    let variables''' = Map.insert (Unicode.collate name) t variables''
    pure (Environment types'' variables''' functions'')

  (Pass2, Syntax.VariableDeclaration {variableId, typeInfo = Nothing, value}) -> do
    let Syntax.Identifier {name} = variableId
    (valueType, Environment types' variables' functions') <- checkExpression environment value
    let variables'' = Map.insert (Unicode.collate name) valueType variables'
    pure (Environment types' variables'' functions')

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
        functions' = Map.empty <| functions

    environment' <- foldlM (checkDeclaration Pass1) (Environment types variables functions') (lefts elements)
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
checkExpression environment expression = case expression of
  Syntax.IntegerExpression {} -> pure (Integer, environment)

  Syntax.RationalExpression {} -> pure (Rational, environment)

  Syntax.VariableExpression {variableId} -> lookupVariable environment variableId

  Syntax.CallExpression {targetId, arguments} -> do
    (argumentTs, environment') <- case arguments of
      Just (first, rest) -> foldlM f ([], environment) ((undefined, first) : rest)
        where
          f (argumentTs, environment) (_, argument) = do
            (t, environment') <- checkExpression environment argument
            pure (argumentTs ++ [t], environment')

      Nothing -> pure ([], environment)

    lookupFunction environment' targetId argumentTs

  Syntax.UnaryExpression {unary, operand} -> do
    (operandType, environment') <- checkExpression environment operand

    case (unary, operandType) of
      (_, Error) -> pure (Error, environment')
      (Syntax.PlusOperator {}, Integer) -> pure (Integer, environment')
      (Syntax.PlusOperator {}, Rational) -> pure (Rational, environment')
      (Syntax.MinusOperator {}, Integer) -> pure (Integer, environment')
      (Syntax.MinusOperator {}, Rational) -> pure (Rational, environment')
      (Syntax.NotOperator {}, Boolean) -> pure (Boolean, environment')

      _ -> do
        tell [InvalidUnaryError unary operandType]
        pure (Error, environment')

  Syntax.BinaryExpression {left, binary, right} -> do
    (leftType, environment') <- checkExpression environment left
    (rightType, environment'') <- checkExpression environment' right

    case (leftType, binary, rightType) of
      (Error, _, _) -> pure (Error, environment'')
      (_, _, Error) -> pure (Error, environment'')
      (Integer, Syntax.AddOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.AddOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.SubtractOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.SubtractOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.MultiplyOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.MultiplyOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.DivideOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.DivideOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.RemainderOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.RemainderOperator {}, Rational) -> pure (Rational, environment'')
      (_, Syntax.EqualOperator {}, _) -> pure (Boolean, environment'')
      (_, Syntax.NotEqualOperator {}, _) -> pure (Boolean, environment'')
      (Integer, Syntax.LessOperator {}, Integer) -> pure (Boolean, environment'')
      (Rational, Syntax.LessOperator {}, Rational) -> pure (Boolean, environment'')
      (Integer, Syntax.LessOrEqualOperator {}, Integer) -> pure (Boolean, environment'')
      (Rational, Syntax.LessOrEqualOperator {}, Rational) -> pure (Boolean, environment'')
      (Integer, Syntax.GreaterOperator {}, Integer) -> pure (Boolean, environment'')
      (Rational, Syntax.GreaterOperator {}, Rational) -> pure (Boolean, environment'')
      (Integer, Syntax.GreaterOrEqualOperator {}, Integer) -> pure (Boolean, environment'')
      (Rational, Syntax.GreaterOrEqualOperator {}, Rational) -> pure (Boolean, environment'')
      (Boolean, Syntax.AndOperator {}, Boolean) -> pure (Boolean, environment'')
      (Boolean, Syntax.OrOperator {}, Boolean) -> pure (Boolean, environment'')

      _ -> do
        tell [InvalidBinaryError binary leftType rightType]
        pure (Error, environment'')

  Syntax.AssignExpression {targetId, assign, value} -> do
    (valueType, environment') <- checkExpression environment value
    (targetType, environment'') <- lookupVariable environment' targetId

    case (targetType, assign, valueType) of
      (Error, _, _) -> pure (Error, environment'')
      (_, _, Error) -> pure (Error, environment'')
      (Unit, Syntax.AssignOperator {}, Unit) -> pure (Unit, environment'')
      (Boolean, Syntax.AssignOperator {}, Boolean) -> pure (Boolean, environment'')
      (Integer, Syntax.AssignOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.AssignOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.AddAssignOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.AddAssignOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.SubtractAssignOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.SubtractAssignOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.MultiplyAssignOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.MultiplyAssignOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.DivideAssignOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.DivideAssignOperator {}, Rational) -> pure (Rational, environment'')
      (Integer, Syntax.RemainderAssignOperator {}, Integer) -> pure (Integer, environment'')
      (Rational, Syntax.RemainderAssignOperator {}, Rational) -> pure (Rational, environment'')

      _ -> do
        tell [InvalidAssignError assign targetType valueType]
        pure (Error, environment'')

  Syntax.ParenthesizedExpression {inner} -> checkExpression environment inner


lookupType :: Environment -> Syntax.Identifier () -> Writer [Error] (Type, Environment)
lookupType environment typeId = do
  let Environment types variables functions = environment
      Syntax.Identifier {name} = typeId
      key = Unicode.collate name

  case Map.insertLookupWithKey (\_ _ old -> old) key Error types of
    (Just t, types') -> pure (t, Environment types' variables functions)

    (Nothing, types') -> do
      tell [UnknownTypeError typeId]
      pure (Unknown name, Environment types' variables functions)


lookupVariable :: Environment -> Syntax.Identifier () -> Writer [Error] (Type, Environment)
lookupVariable environment variableId = do
  let Environment types variables functions = environment
      Syntax.Identifier {name} = variableId
      key = Unicode.collate name

  case Map.insertLookupWithKey (\_ _ old -> old) key Error variables of
    (Just t, variables') -> pure (t, Environment types variables' functions)

    (Nothing, variables') -> do
      tell [UnknownIdentifierError variableId]
      pure (Error, Environment types variables' functions)


lookupFunction :: Environment -> Syntax.Identifier () -> [Type] -> Writer [Error] (Type, Environment)
lookupFunction environment functionId parameterTypes = go functions
  where
    Environment types variables functions = environment
    Syntax.Identifier {name} = functionId
    key = Unicode.collate name

    go functions = do
      let (head, tail) = NonEmpty.uncons functions

      case lookup parameterTypes =<< Map.lookup key head of
        Just _ | any isError parameterTypes -> pure (Error, environment)

        Just returnType -> pure (returnType, environment)

        Nothing -> case tail of
          Just tail -> go tail

          Nothing -> do
            tell [UnknownFunctionError functionId parameterTypes]
            pure (Error, Environment types variables (Map.singleton key [(parameterTypes, Error)] :| []))
