module Type (
  Environment (..),
  Type (..),
  Error (..),
  description,
  span,
  start,
  end,
  defaultEnvironment,
  checkDeclarations,
  checkDeclaration,
  checkStatement,
  checkExpression,
  checkIdentifier
) where

import Control.Monad
import Data.Either
import Data.Foldable
import Data.Functor
import Data.List hiding (span)
import Data.Maybe
import Prelude hiding (span)

import Control.Monad.Trans.Writer

import Data.Text (Text)
import qualified Data.Text as Text

import Span (Span)
import qualified Syntax
import qualified Unicode


data Environment where
  Environment :: [(Text, Type, Bool)] -> [(Text, [Type], Type)] -> Environment
  deriving (Eq, Show, Read)


data Type where
  Error :: Type
  Int :: Type
  Float :: Type
  Bool :: Type
  Unit :: Type
  Function :: [Type] -> Type -> Type
  deriving (Eq, Show, Read)


data Error where
  UnknownTypeError :: Syntax.Identifier -> Error
  UnknownIdentifierError :: Syntax.Identifier -> Error
  UnknownFunctionError :: Syntax.Identifier -> [Type] -> Error
  UnassignedVariableError :: Syntax.Identifier -> Error
  InvalidUnaryError :: Syntax.UnaryOperator -> Type -> Error
  InvalidBinaryError :: Syntax.BinaryOperator -> Type -> Type -> Error
  InvalidAssignError :: Syntax.AssignOperator -> Type -> Type -> Error
  InvalidReturnTypeError :: Syntax.Statement () -> Type -> Type -> Error
  MissingReturnValueError :: Syntax.Statement () -> Type -> Error
  InvalidTypeError :: Syntax.Expression () -> Type -> Type -> Error
  deriving (Eq, Show, Read)


description :: Error -> Text

description (UnknownTypeError (Syntax.Identifier _ name)) =
  "Unknown type " <> name

description (UnknownIdentifierError (Syntax.Identifier _ name)) =
  "Unknown identifier " <> name

description (UnknownFunctionError (Syntax.Identifier _ name) parameterTs) =
  "Unknown function " <> name <> "(" <> Text.pack (intercalate ", " (show <$> parameterTs)) <> ")"

description (UnassignedVariableError (Syntax.Identifier _ name)) =
  "Unassigned variable " <> name

description (InvalidUnaryError unary operandT) =
  "Cant' apply unary " <> operator <> " to " <> Text.pack (show operandT)
  where
    operator = case unary of
      Syntax.PlusOperator _ -> "+"
      Syntax.MinusOperator _ -> "-"
      Syntax.NotOperator _ -> "not"

description (InvalidBinaryError binary leftT rightT) =
  "Can't apply binary " <> operator <> " to " <> Text.pack (show leftT) <> " and " <> Text.pack (show rightT)
  where
    operator = case binary of
      Syntax.AddOperator _ -> "+"
      Syntax.SubtractOperator _ -> "-"
      Syntax.MultiplyOperator _ -> "*"
      Syntax.DivideOperator _ -> "/"
      Syntax.RemainderOperator _ -> "%"
      Syntax.EqualOperator _ -> "=="
      Syntax.NotEqualOperator _ -> "!="
      Syntax.LessOperator _ -> "<"
      Syntax.LessOrEqualOperator _ -> "<="
      Syntax.GreaterOperator _ -> ">"
      Syntax.GreaterOrEqualOperator _ -> ">="
      Syntax.AndOperator _ -> "and"
      Syntax.OrOperator _ -> "or"

description (InvalidAssignError assign targetT valueT) =
  "Can't apply assign " <> operator <> " to " <> Text.pack (show targetT) <> " and " <> Text.pack (show valueT)
  where
    operator = case assign of
      Syntax.AssignOperator _ -> "="
      Syntax.AddAssignOperator _ -> "+="
      Syntax.SubtractAssignOperator _ -> "-="
      Syntax.MultiplyAssignOperator _ -> "*="
      Syntax.DivideAssignOperator _ -> "/="
      Syntax.RemainderAssignOperator _ -> "%="

description (InvalidReturnTypeError _ expectedT resultT) =
  "Invalid return type: expected " <> Text.pack (show expectedT) <> ", but got " <> Text.pack (show resultT)

description (MissingReturnValueError _ expectedT) =
  "Missing return value: expected " <> Text.pack (show expectedT)

description (InvalidTypeError _ expectedT actualT) =
  "Invalid type: expected " <> Text.pack (show expectedT) <> ", but got " <> Text.pack (show actualT)


span :: Error -> Span
span (UnknownTypeError tName) = Syntax.span tName
span (UnknownIdentifierError name) = Syntax.span name
span (UnknownFunctionError name _) = Syntax.span name
span (UnassignedVariableError variable) = Syntax.span variable
span (InvalidUnaryError unary _) = Syntax.span unary
span (InvalidBinaryError binary _ _) = Syntax.span binary
span (InvalidAssignError assign _ _) = Syntax.span assign
span (InvalidReturnTypeError statement _ _) = Syntax.span statement
span (MissingReturnValueError statement _) = Syntax.span statement
span (InvalidTypeError expression _ _) = Syntax.span expression


start :: Integral a => Error -> a
start (UnknownTypeError tName) = Syntax.start tName
start (UnknownIdentifierError name) = Syntax.start name
start (UnknownFunctionError name _) = Syntax.start name
start (UnassignedVariableError variable) = Syntax.start variable
start (InvalidUnaryError unary _) = Syntax.start unary
start (InvalidBinaryError binary _ _) = Syntax.start binary
start (InvalidAssignError assign _ _) = Syntax.start assign
start (InvalidReturnTypeError statement _ _) = Syntax.start statement
start (MissingReturnValueError statement _) = Syntax.start statement
start (InvalidTypeError expression _ _) = Syntax.start expression


end :: Integral a => Error -> a
end (UnknownTypeError tName) = Syntax.end tName
end (UnknownIdentifierError name) = Syntax.end name
end (UnknownFunctionError name _) = Syntax.end name
end (UnassignedVariableError variable) = Syntax.end variable
end (InvalidUnaryError unary _) = Syntax.end unary
end (InvalidBinaryError binary _ _) = Syntax.end binary
end (InvalidAssignError assign _ _) = Syntax.end assign
end (InvalidReturnTypeError statement _ _) = Syntax.end statement
end (MissingReturnValueError statement _) = Syntax.end statement
end (InvalidTypeError expression _ _) = Syntax.end expression


defaultEnvironment :: Environment
defaultEnvironment = Environment ts []
  where
    ts =
      [
        (Unicode.collate "unit", Unit, True),
        (Unicode.collate "true", Bool, True),
        (Unicode.collate "false", Bool, True)
      ]


checkDeclarations :: Foldable t => Environment -> t (Syntax.Declaration ()) -> [Error]
checkDeclarations environment declarations = execWriter (checkDeclarationsW environment declarations)


checkDeclaration :: Environment -> Syntax.Declaration () -> (Environment, [Error])
checkDeclaration environment declaration = runWriter (checkDeclarationW environment declaration)


checkStatement :: Type -> Environment -> Syntax.Statement () -> (Bool, Environment, [Error])
checkStatement extectedT environment statement =
  let ((doesReturn, environment'), errors) = runWriter (checkStatementW extectedT environment statement)
   in (doesReturn, environment', errors)


checkExpression :: Environment -> Syntax.Expression () -> (Type, Environment, [Error])
checkExpression environment expression =
  let ((t, environment'), errors) = runWriter (checkExpressionW environment expression)
   in (t, environment', errors)


checkIdentifier :: Bool -> Environment -> Syntax.Identifier -> (Type, Environment, [Error])
checkIdentifier expectInitialized environment identifier =
  let ((t, environment'), errors) = runWriter (lookupT expectInitialized environment identifier)
   in (t, environment', errors)


checkDeclarationsW :: Foldable t => Environment -> t (Syntax.Declaration ()) -> Writer [Error] Environment
checkDeclarationsW environment declarations = do
  (Environment ts' functionTs') <- declareFunctions environment declarations
  let f (Environment ts _) declaration = checkDeclarationW (Environment ts functionTs') declaration
  foldlM f (Environment ts' functionTs') declarations


checkDeclarationW :: Environment -> Syntax.Declaration () -> Writer [Error] Environment

checkDeclarationW (Environment ts functionTs) Syntax.EmptyVariableDeclaration {variable, tName} = do
  let (Syntax.Identifier _ variableName) = variable
  t <- getT tName
  pure (Environment ((Unicode.collate variableName, t, False) : ts) functionTs)

checkDeclarationW environment Syntax.VariableDeclaration {variable, tName, value} = do
  let (Syntax.Identifier _ variableName) = variable
  t <- getT tName
  (valueT, Environment ts' functionTs') <- checkExpressionW environment value
  when (t /= Error && valueT /= Error && valueT /= t) (tell [InvalidTypeError value t valueT])
  pure (Environment ((Unicode.collate variableName, t, True) : ts') functionTs')

checkDeclarationW (Environment ts functionTs) Syntax.FunctionDeclaration {name = n, parameters, tName, body} = do
  let (Syntax.Identifier _ name) = n

  (parameterTs, ts') <- case parameters of
    Just ((name, _, tName), rest) -> foldlM f ([], ts) ((name, tName) : [(name, tName) | (_, name, _, tName) <- rest])
      where
        f (parameterTs, ts) (Syntax.Identifier _ name, tName) = do
          t <- getT tName
          pure (t : parameterTs, (Unicode.collate name, t, True) : ts)

    Nothing -> pure ([], ts)

  returnT <- getT tName
  let functionTs' = (Unicode.collate name, parameterTs, returnT) : functionTs
  (doesReturn, _) <- checkStatementW returnT (Environment ts' functionTs') body
  when (returnT `notElem` [Error, Unit] && not doesReturn) (tell [MissingReturnValueError body returnT])
  pure (Environment ts functionTs')


checkStatementW :: Type -> Environment -> Syntax.Statement () -> Writer [Error] (Bool, Environment)

checkStatementW _ environment Syntax.ExpressionStatement {value} = do
  (_, environment') <- checkExpressionW environment value
  pure (False, environment')

checkStatementW expectedT environment Syntax.IfStatement {predicate, trueBranch} = do
  (predicateT, environment') <- checkExpressionW environment predicate
  when (predicateT /= Bool) (tell [InvalidTypeError predicate Bool predicateT])
  (_, environment'') <- checkStatementW expectedT environment' trueBranch
  pure (False, environment'')

checkStatementW expectedT environment Syntax.IfElseStatement {predicate, trueBranch, falseBranch} = do
  (predicateT, environment') <- checkExpressionW environment predicate
  when (predicateT /= Bool) (tell [InvalidTypeError predicate Bool predicateT])
  (doesTrueBranchReturn, environment'') <- checkStatementW expectedT environment' trueBranch
  (doesFalseBrachReturn, environment''') <- checkStatementW expectedT environment'' falseBranch
  pure (doesTrueBranchReturn && doesFalseBrachReturn, environment''')

checkStatementW expectedT environment Syntax.WhileStatement {predicate, body} = do
  (predicateT, environment') <- checkExpressionW environment predicate
  when (predicateT /= Bool) (tell [InvalidTypeError predicate Bool predicateT])
  (_, environment'') <- checkStatementW expectedT environment' body
  pure (False, environment'')

checkStatementW expectedT environment Syntax.DoWhileStatement {body, predicate} = do
  (doesReturn, environment') <- checkStatementW expectedT environment body
  (predicateT, environment'') <- checkExpressionW environment' predicate
  when (predicateT /= Bool) (tell [InvalidTypeError predicate Bool predicateT])
  pure (doesReturn, environment'')

checkStatementW expectedT environment s @ Syntax.ReturnStatement {result = Just result} = do
  (resultT, environment') <- checkExpressionW environment result

  when (expectedT /= Error && resultT /= Error && resultT /= expectedT) $
    tell [InvalidReturnTypeError s expectedT resultT]

  pure (True, environment')

checkStatementW Error environment Syntax.ReturnStatement {result = Nothing} = pure (True, environment)

checkStatementW Unit environment Syntax.ReturnStatement {result = Nothing} = pure (True, environment)

checkStatementW expectedT environment s @ Syntax.ReturnStatement {result = Nothing} = do
  tell [MissingReturnValueError s expectedT]
  pure (True, environment)

checkStatementW expectedT environment Syntax.BlockStatement {elements} = do
  (Environment ts' functionTs') <- declareFunctions environment (lefts elements)

  let f (doesReturn, Environment ts _) (Left declaration) = do
        (Environment ts' _) <- checkDeclarationW (Environment ts functionTs') declaration
        pure (doesReturn, Environment ts' functionTs')

      f (doesReturn, Environment ts _) (Right statement) = do
        (doesReturn', Environment ts' _) <- checkStatementW expectedT (Environment ts functionTs') statement
        pure (doesReturn || doesReturn', Environment ts' functionTs')

  foldlM f (False, Environment ts' functionTs') elements


checkExpressionW :: Environment -> Syntax.Expression () -> Writer [Error] (Type, Environment)

checkExpressionW environment Syntax.IntegerExpression {} = pure (Int, environment)

checkExpressionW environment Syntax.VariableExpression {variable} = lookupT True environment variable

checkExpressionW environment Syntax.CallExpression {target, arguments} = do
  (argumentTs, environment') <- case arguments of
    Just (first, rest) -> foldlM f ([], environment) (first : map snd rest)
      where
        f (argumentTs, environment) argument = do
          (argumentT, environment') <- checkExpressionW environment argument
          pure (argumentTs ++ [argumentT], environment')

    Nothing -> pure ([], environment)

  if Error `notElem` argumentTs then
    lookupFunctionT environment' target argumentTs
  else
    pure (Error, environment')

checkExpressionW environment Syntax.UnaryExpression {unary, operand} = do
  (operandT, environment') <- checkExpressionW environment operand

  case (unary, operandT) of
    (_, Error) -> pure (Error, environment')
    (Syntax.PlusOperator _, Int) -> pure (Int, environment')
    (Syntax.PlusOperator _, Float) -> pure (Float, environment')
    (Syntax.MinusOperator _, Int) -> pure (Int, environment')
    (Syntax.MinusOperator _, Float) -> pure (Float, environment')
    (Syntax.NotOperator _, Bool) -> pure (Bool, environment')

    _ -> do
      tell [InvalidUnaryError unary operandT]
      pure (Error, environment')

checkExpressionW environment Syntax.BinaryExpression {left, binary, right} = do
  (leftT, environment') <- checkExpressionW environment left
  (rightT, environment'') <- checkExpressionW environment' right

  case (leftT, binary, rightT) of
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
    (Int, Syntax.EqualOperator _, Int) -> pure (Bool, environment'')
    (Float, Syntax.EqualOperator _, Float) -> pure (Bool, environment'')
    (Bool, Syntax.EqualOperator _, Bool) -> pure (Bool, environment'')
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
      tell [InvalidBinaryError binary leftT rightT]
      pure (Error, environment'')

checkExpressionW environment Syntax.AssignExpression {target, assign, value} = do
  (targetT, environment') <- lookupT True environment target
  (valueT, environment'') <- checkExpressionW environment' value

  case (targetT, assign, valueT) of
    (Error, _, _) -> pure (Error, environment'')
    (_, _, Error) -> pure (Error, environment'')
    (Int, Syntax.AssignOperator _, Int) -> pure (Int, environment'')
    (Float, Syntax.AssignOperator _, Float) -> pure (Float, environment'')
    (Bool, Syntax.AssignOperator _, Bool) -> pure (Bool, environment'')
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
      tell [InvalidAssignError assign targetT valueT]
      pure (Error, environment'')

checkExpressionW environment Syntax.ParenthesizedExpression {inner} = checkExpressionW environment inner


declareFunctions :: Foldable t => Environment -> t (Syntax.Declaration a) -> Writer [Error] Environment
declareFunctions = foldlM f
  where
    f (Environment ts functionTs) Syntax.FunctionDeclaration {name = (Syntax.Identifier _ name), parameters, tName} = do
      parameterTs <- case parameters of
        Just ((_, _, tName), rest) -> foldlM g [] (tName : [tName | (_, _, _, tName) <- rest])
        Nothing -> pure []

      returnT <- getT tName
      pure (Environment ts ((Unicode.collate name, parameterTs, returnT) : functionTs))

    f (Environment ts functionTs) _ = pure (Environment ts functionTs)

    g parameterTs tName = do
      t <- getT tName
      pure (t : parameterTs)


lookupT :: Bool -> Environment -> Syntax.Identifier -> Writer [Error] (Type, Environment)
lookupT expectInitialized environment @ (Environment ts functionTs) identifier @ (Syntax.Identifier _ name) =
  case lookup (Unicode.collate name) ts of
    Just (t, True) -> pure (t, environment)

    Just (t, False) | not expectInitialized -> pure (t, environment)

    Just (t, False) -> do
      tell [UnassignedVariableError identifier]
      pure (t, Environment ((Unicode.collate name, t, expectInitialized) : ts) functionTs)

    Nothing -> do
      tell [UnknownIdentifierError identifier]
      pure (Error, Environment ((Unicode.collate name, Error, expectInitialized) : ts) functionTs)

  where
    lookup _ [] = Nothing
    lookup name ((name', t, isInitialized) : _) | name' == name = Just (t, isInitialized)
    lookup name (_ : ts) = lookup name ts


lookupFunctionT :: Environment -> Syntax.Identifier -> [Type] -> Writer [Error] (Type, Environment)
lookupFunctionT environment @ (Environment ts functionTs) identifier @ (Syntax.Identifier _ name) parameterTs =
  case lookup (Unicode.collate name) parameterTs functionTs of
    Just t -> pure (t, environment)

    Nothing -> do
      tell [UnknownFunctionError identifier parameterTs]
      pure (Error, Environment ts ((Unicode.collate name, parameterTs, Error) : functionTs))

  where
    lookup _ _ [] = Nothing
    lookup name parameters ((name', parameters', t) : _) | name' == name && parameters' == parameters = Just t
    lookup name parameters (_ : parameterTs) = lookup name parameters parameterTs


getT :: Syntax.Identifier -> Writer [Error] Type
getT identifier @ (Syntax.Identifier _ tName)
  | Unicode.collate tName == Unicode.collate "Int" = pure Int
  | Unicode.collate tName == Unicode.collate "Float" = pure Float
  | Unicode.collate tName == Unicode.collate "Bool" = pure Bool
  | Unicode.collate tName == Unicode.collate "Unit" = pure Unit
  | otherwise = tell [UnknownTypeError identifier] $> Error
