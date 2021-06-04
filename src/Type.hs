module Type (
  Type (..),
  Pass (..),
  Environment (..),
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

import Prelude hiding (span)
import Data.Functor.Classes
import Control.Monad
import Data.Either
import Data.Foldable

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad.Trans.Writer

import Span (Span)
import qualified Syntax
import qualified Unicode


data Type where
  Unit :: Type
  Bool :: Type
  Int :: Type
  Float :: Type
  Function :: [Type] -> Type -> Type
  Unknown :: Text -> Type
  Error :: Type
  deriving (Eq, Show, Read)


data Pass where
  Pass1 :: Pass
  Pass2 :: Pass
  deriving (Eq, Ord, Enum, Bounded, Show, Read)


data Environment where
  Environment :: [(Text, Type)] -> [(Text, Type, Bool)] -> [[(Text, [Type], Type)]] -> Environment
  deriving (Eq, Show, Read)


data Error where
  UnknownTypeError :: Syntax.Identifier -> Error
  UnknownIdentifierError :: Syntax.Identifier -> Error
  UnknownFunctionError :: Syntax.Identifier -> [Type] -> Error
  DuplicateFunctionDefinition :: Syntax.Identifier -> [Type] -> Error
  UninitializedVariableError :: Syntax.Identifier -> Error
  InvalidUnaryError :: Syntax.UnaryOperator -> Type -> Error
  InvalidBinaryError :: Syntax.BinaryOperator -> Type -> Type -> Error
  InvalidAssignError :: Syntax.AssignOperator -> Type -> Type -> Error
  InvalidTypeError :: Syntax.Expression () -> Type -> Type -> Error
  InvalidReturnTypeError :: Syntax.Statement () -> Type -> Type -> Error
  MissingReturnValueError :: Syntax.Statement () -> Type -> Error
  MissingReturnPathError :: Syntax.Identifier -> [Type] -> Error
  deriving (Eq, Show, Read)


text :: Type -> Text
text Unit = "Unit"
text Bool = "Bool"
text Int = "Int"
text Float = "Float"
text (Function parameterTs returnT) = "(" <> Text.intercalate ", " (text <$> parameterTs) <> ") -> " <> text returnT
text (Unknown name) = name
text Error = "⊥"


isCompatible :: Type -> Type -> Bool
isCompatible Error _ = True
isCompatible _ Error = True
isCompatible t1 t2 = t1 == t2


description :: Error -> Text

description (UnknownTypeError (Syntax.Identifier _ name)) = "Unknown type " <> name

description (UnknownIdentifierError (Syntax.Identifier _ name)) = "Unknown identifier " <> name

description (UnknownFunctionError (Syntax.Identifier _ name) parameterTs) =
  "Unknown function " <> name <> "(" <> Text.intercalate ", " (text <$> parameterTs) <> ")"

description (DuplicateFunctionDefinition (Syntax.Identifier _ name) parameterTs) =
  "Function " <> name <> "(" <> Text.intercalate ", " (text <$> parameterTs) <> ") already defined"

description (UninitializedVariableError (Syntax.Identifier _ name)) = "Uninitialized variable " <> name

description (InvalidUnaryError unary operandT) = "Can’t apply unary " <> operator <> " to " <> text operandT
  where
    operator = case unary of
      Syntax.PlusOperator _ -> "+"
      Syntax.MinusOperator _ -> "-"
      Syntax.NotOperator _ -> "not"

description (InvalidBinaryError binary leftT rightT) =
  "Can’t apply binary " <> operator <> " between " <> text leftT <> " and " <> text rightT
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
  "Can’t apply assign " <> operator <> " to " <> text targetT <> " and " <> text valueT
  where
    operator = case assign of
      Syntax.AssignOperator _ -> "="
      Syntax.AddAssignOperator _ -> "+="
      Syntax.SubtractAssignOperator _ -> "-="
      Syntax.MultiplyAssignOperator _ -> "*="
      Syntax.DivideAssignOperator _ -> "/="
      Syntax.RemainderAssignOperator _ -> "%="

description (InvalidTypeError _ expectedT actualT) =
  "Invalid type: expected " <> text expectedT <> ", but got " <> text actualT

description (InvalidReturnTypeError _ expectedT resultT) =
  "Invalid return type: expected " <> text expectedT <> ", but got " <> text resultT

description (MissingReturnValueError _ expectedT) = "Missing return value: expected " <> text expectedT

description (MissingReturnPathError (Syntax.Identifier _ name) parameterTs) =
  name <> "(" <> Text.intercalate ", " (text <$> parameterTs) <> "): not all code paths return a value"


span :: Error -> Span
span (UnknownTypeError tName) = Syntax.span tName
span (UnknownIdentifierError name) = Syntax.span name
span (UnknownFunctionError name _) = Syntax.span name
span (DuplicateFunctionDefinition name _) = Syntax.span name
span (UninitializedVariableError variable) = Syntax.span variable
span (InvalidUnaryError unary _) = Syntax.span unary
span (InvalidBinaryError binary _ _) = Syntax.span binary
span (InvalidAssignError assign _ _) = Syntax.span assign
span (InvalidTypeError expression _ _) = Syntax.span expression
span (InvalidReturnTypeError statement _ _) = Syntax.span statement
span (MissingReturnValueError statement _) = Syntax.span statement
span (MissingReturnPathError name _) = Syntax.span name


start :: Integral a => Error -> a
start (UnknownTypeError tName) = Syntax.start tName
start (UnknownIdentifierError name) = Syntax.start name
start (UnknownFunctionError name _) = Syntax.start name
start (DuplicateFunctionDefinition name _) = Syntax.start name
start (UninitializedVariableError variable) = Syntax.start variable
start (InvalidUnaryError unary _) = Syntax.start unary
start (InvalidBinaryError binary _ _) = Syntax.start binary
start (InvalidAssignError assign _ _) = Syntax.start assign
start (InvalidTypeError expression _ _) = Syntax.start expression
start (InvalidReturnTypeError statement _ _) = Syntax.start statement
start (MissingReturnValueError statement _) = Syntax.start statement
start (MissingReturnPathError name _) = Syntax.start name


end :: Integral a => Error -> a
end (UnknownTypeError tName) = Syntax.end tName
end (UnknownIdentifierError name) = Syntax.end name
end (UnknownFunctionError name _) = Syntax.end name
end (DuplicateFunctionDefinition name _) = Syntax.end name
end (UninitializedVariableError variable) = Syntax.end variable
end (InvalidUnaryError unary _) = Syntax.end unary
end (InvalidBinaryError binary _ _) = Syntax.end binary
end (InvalidAssignError assign _ _) = Syntax.end assign
end (InvalidTypeError expression _ _) = Syntax.end expression
end (InvalidReturnTypeError statement _ _) = Syntax.end statement
end (MissingReturnValueError statement _) = Syntax.end statement
end (MissingReturnPathError name _) = Syntax.end name


defaultEnvironment :: Environment
defaultEnvironment = Environment ts variableTs functionTs
  where
    ts =
      [
        (Unicode.collate "Unit", Unit),
        (Unicode.collate "Bool", Bool),
        (Unicode.collate "Int", Int),
        (Unicode.collate "Float", Float)
      ]

    variableTs =
      [
        (Unicode.collate "unit", Unit, True),
        (Unicode.collate "true", Bool, True),
        (Unicode.collate "false", Bool, True)
      ]

    functionTs =
      [
        [],

        [
          (Unicode.collate "int", [Int], Int),
          (Unicode.collate "int", [Float], Int),
          (Unicode.collate "float", [Int], Float),
          (Unicode.collate "float", [Float], Float)
        ]
      ]


checkDeclarations :: Foldable t => Environment -> t (Syntax.Declaration ()) -> [Error]
checkDeclarations environment declarations = execWriter (checkDeclarationsW environment declarations)


checkDeclaration :: Pass -> Environment -> Syntax.Declaration () -> (Environment, [Error])
checkDeclaration pass environment declaration = runWriter (checkDeclarationW pass environment declaration)


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
  let ((t, environment'), errors) = runWriter (lookupVariableTW expectInitialized environment identifier)
   in (t, environment', errors)


checkDeclarationsW :: Foldable t => Environment -> t (Syntax.Declaration ()) -> Writer [Error] Environment
checkDeclarationsW environment declarations = do
  environment' <- foldlM (checkDeclarationW Pass1) environment declarations
  foldlM (checkDeclarationW Pass2) environment' declarations


checkDeclarationW :: Pass -> Environment -> Syntax.Declaration () -> Writer [Error] Environment

checkDeclarationW Pass2 environment Syntax.EmptyVariableDeclaration {variable, tName} = do
  let Syntax.Identifier _ variableName = variable
  (t, Environment ts' variableTs' functionTs') <- getTW environment tName
  pure (Environment ts' ((Unicode.collate variableName, t, False) : variableTs') functionTs')

checkDeclarationW Pass2 environment Syntax.VariableDeclaration {variable, tName, value} = do
  let Syntax.Identifier _ variableName = variable
  (t, environment') <- getTW environment tName
  (valueT, Environment ts'' variableTs'' functionTs'') <- checkExpressionW environment' value
  unless (isCompatible valueT t) (tell [InvalidTypeError value t valueT])
  pure (Environment ts'' ((Unicode.collate variableName, t, True) : variableTs'') functionTs'')

checkDeclarationW pass environment Syntax.FunctionDeclaration {name = n, parameters, tName, body} = do
  let Environment _ variableTs functionTs = environment
      Syntax.Identifier _ name = n

  (parameterTs, environment') <- case parameters of
    Just ((name, _, tName), rest) ->
      foldlM f ([], environment) ((name, tName) : [(n, tn) | (_, n, _, tn) <- rest])
      where
        f (parameterTs, environment) (Syntax.Identifier _ name, tName) = do
          (t, Environment ts' variableTs' functionTs') <- getTW environment tName
          pure (parameterTs ++ [t], Environment ts' ((Unicode.collate name, t, True) : variableTs') functionTs')

    Nothing -> pure ([], environment)

  (returnT, Environment ts'' variableTs'' functionTs'') <- getTW environment' tName

  case pass of
    Pass1 -> do
      let key = Unicode.collate name

      if any (\(k, pts, _) -> k == key && liftEq isCompatible pts parameterTs) (head functionTs'') then do
        tell [DuplicateFunctionDefinition n parameterTs]
        pure (Environment ts'' variableTs functionTs'')
      else
        pure (Environment ts'' variableTs (((key, parameterTs, returnT) : head functionTs'') : tail functionTs''))

    Pass2 -> do
      (doesReturn, _) <- checkStatementW returnT (Environment ts'' variableTs'' functionTs) body
      unless (isCompatible returnT Unit || doesReturn) (tell [MissingReturnPathError n parameterTs])
      pure (Environment ts'' variableTs functionTs)

checkDeclarationW Pass1 environment _ = pure environment


checkStatementW :: Type -> Environment -> Syntax.Statement () -> Writer [Error] (Bool, Environment)

checkStatementW _ environment Syntax.ExpressionStatement {value} = do
  (_, environment') <- checkExpressionW environment value
  pure (False, environment')

checkStatementW expectedT environment Syntax.IfStatement {predicate, trueBranch} = do
  (predicateT, environment') <- checkExpressionW environment predicate
  unless (isCompatible predicateT Bool) (tell [InvalidTypeError predicate Bool predicateT])
  (_, environment'') <- checkStatementW expectedT environment' trueBranch
  pure (False, environment'')

checkStatementW expectedT environment Syntax.IfElseStatement {predicate, trueBranch, falseBranch} = do
  (predicateT, environment') <- checkExpressionW environment predicate
  unless (isCompatible predicateT Bool) (tell [InvalidTypeError predicate Bool predicateT])
  (doesTrueBranchReturn, environment'') <- checkStatementW expectedT environment' trueBranch
  (doesFalseBrachReturn, environment''') <- checkStatementW expectedT environment'' falseBranch
  pure (doesTrueBranchReturn && doesFalseBrachReturn, environment''')

checkStatementW expectedT environment Syntax.WhileStatement {predicate, body} = do
  (predicateT, environment') <- checkExpressionW environment predicate
  unless (isCompatible predicateT Bool) (tell [InvalidTypeError predicate Bool predicateT])
  (_, environment'') <- checkStatementW expectedT environment' body
  pure (False, environment'')

checkStatementW expectedT environment Syntax.DoWhileStatement {body, predicate} = do
  (doesReturn, environment') <- checkStatementW expectedT environment body
  (predicateT, environment'') <- checkExpressionW environment' predicate
  unless (isCompatible predicateT Bool) (tell [InvalidTypeError predicate Bool predicateT])
  pure (doesReturn, environment'')

checkStatementW expectedT environment s @ Syntax.ReturnStatement {result = Just result} = do
  (resultT, environment') <- checkExpressionW environment result
  unless (isCompatible resultT expectedT) (tell [InvalidReturnTypeError s expectedT resultT])
  pure (True, environment')

checkStatementW expectedT environment s @ Syntax.ReturnStatement {result = Nothing} = do
  unless (isCompatible expectedT Unit) (tell [MissingReturnValueError s expectedT])
  pure (True, environment)

checkStatementW expectedT environment Syntax.BlockStatement {elements} = do
  let Environment ts variableTs functionTs = environment
  environment' <- foldlM (checkDeclarationW Pass1) (Environment ts variableTs ([] : functionTs)) (lefts elements)
  (doesReturn, _) <- foldlM f (False, environment') elements
  pure (doesReturn, environment)
  where
    f (doesReturn, environment) (Left declaration) = do
      environment' <- checkDeclarationW Pass2 environment declaration
      pure (doesReturn, environment')

    f (doesReturn, environment) (Right statement) = do
      (doesReturn', environment') <- checkStatementW expectedT environment statement
      pure (doesReturn || doesReturn', environment')


checkExpressionW :: Environment -> Syntax.Expression () -> Writer [Error] (Type, Environment)

checkExpressionW environment Syntax.IntegerExpression {} = pure (Int, environment)

checkExpressionW environment Syntax.VariableExpression {variable} = lookupVariableTW True environment variable

checkExpressionW environment Syntax.CallExpression {target, arguments} = do
  (argumentTs, environment') <- case arguments of
    Just (first, rest) -> foldlM f ([], environment) (first : map snd rest)
      where
        f (argumentTs, environment) argument = do
          (argumentT, environment') <- checkExpressionW environment argument
          pure (argumentTs ++ [argumentT], environment')

    Nothing -> pure ([], environment)

  lookupFunctionTW environment' target argumentTs

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
    (Unit, Syntax.EqualOperator _, Unit) -> pure (Bool, environment'')
    (Bool, Syntax.EqualOperator _, Bool) -> pure (Bool, environment'')
    (Int, Syntax.EqualOperator _, Int) -> pure (Bool, environment'')
    (Float, Syntax.EqualOperator _, Float) -> pure (Bool, environment'')
    (Unit, Syntax.NotEqualOperator _, Unit) -> pure (Bool, environment'')
    (Bool, Syntax.NotEqualOperator _, Bool) -> pure (Bool, environment'')
    (Int, Syntax.NotEqualOperator _, Int) -> pure (Bool, environment'')
    (Float, Syntax.NotEqualOperator _, Float) -> pure (Bool, environment'')
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
  (valueT, environment') <- checkExpressionW environment value

  (targetT, environment'') <- case assign of
    Syntax.AssignOperator _ -> lookupVariableTW False environment' target
    _ -> lookupVariableTW True environment' target

  case (targetT, assign, valueT) of
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
      tell [InvalidAssignError assign targetT valueT]
      pure (Error, environment'')

checkExpressionW environment Syntax.ParenthesizedExpression {inner} = checkExpressionW environment inner


lookupVariableTW :: Bool -> Environment -> Syntax.Identifier -> Writer [Error] (Type, Environment)
lookupVariableTW expectInitialized environment (Syntax.Identifier span name) = do
  let Environment ts variableTs functionTs = environment
      key = Unicode.collate name

  case find (\(k, _, _) -> k == key) variableTs of
    Just (_, t, True) -> pure (t, environment)

    Just (_, t, False) | expectInitialized -> do
      tell [UninitializedVariableError (Syntax.Identifier span name)]
      pure (t, Environment ts ((key, t, True) : variableTs) functionTs)

    Just (_, t, False) -> pure (t, Environment ts ((name, t, True) : variableTs) functionTs)

    Nothing -> do
      tell [UnknownIdentifierError (Syntax.Identifier span name)]
      pure (Error, Environment ts ((key, Error, expectInitialized) : variableTs) functionTs)


lookupFunctionTW :: Environment -> Syntax.Identifier -> [Type] -> Writer [Error] (Type, Environment)
lookupFunctionTW environment (Syntax.Identifier span name) parameterTs = go functionTs
  where
    Environment ts variableTs functionTs = environment
    key = Unicode.collate name

    go [] = do
      tell [UnknownFunctionError (Syntax.Identifier span name) parameterTs]
      pure (Error, Environment ts variableTs (((key, parameterTs, Error) : head functionTs) : tail functionTs))

    go functionTs = case find (\(k, pts, _) -> k == key && liftEq isCompatible pts parameterTs) (head functionTs) of
      Just (_, _, returnT) | Error `notElem` parameterTs -> pure (returnT, environment)
      Just _ -> pure (Error, environment)
      Nothing -> go (tail functionTs)


getTW :: Environment -> Syntax.Identifier -> Writer [Error] (Type, Environment)
getTW environment (Syntax.Identifier span tName) = do
  let Environment ts variableTs functionTs = environment
      key = Unicode.collate tName

  case lookup key ts of
    Just t -> pure (t, environment)

    Nothing -> do
      tell [UnknownTypeError (Syntax.Identifier span tName)]
      pure (Unknown tName, Environment ((key, Error) : ts) variableTs functionTs)
