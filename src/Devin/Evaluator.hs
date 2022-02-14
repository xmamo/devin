{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Devin.Evaluator (
  Evaluator (..),
  Result (..),
  OnSyntax (..),
  State,
  Frame (..),
  Function (..),
  Value (..),
  Reference,
  makePredefinedState,
  runEvaluatorStep,
  runEvaluator,
  newReference,
  readReference,
  writeReference,
  cloneReference,
  compareReferences,
  compareValues,
  defineFunction,
  lookupFunction,
  defineVariable,
  lookupVariable,
  withNewFrame,
  raise,
  yield,
  onDeclaration,
  onStatement,
  onExpression
) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Data
import Data.Int
import Data.IORef

import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector

import Devin.Error
import Devin.Interval
import Devin.Syntax


newtype Evaluator a = Evaluator (State -> IO (Result a, State))
  deriving Functor


data Result a
  = Done a
  | Step OnSyntax (Evaluator a)
  | Error Error
  deriving Functor


data OnSyntax
  = BeforeDeclaration Declaration
  | AfterDeclaration Declaration
  | BeforeStatement Statement
  | AfterStatement Statement
  | BeforeExpression Expression
  | AfterExpression Expression
  deriving (Eq, Read, Show, Data)


type State = [Frame]


data Frame = Frame {
  offset :: Int,  -- Access link / static link
  functions :: [(String, Function)],
  variables :: [(String, Reference)]
}


data Function
  = UserDefined Declaration
  | BuiltinToInt
  | BuiltinToFloat
  deriving (Eq, Show, Read, Data)


data Value
  = Unit
  | Bool Bool
  | Int Int64
  | Float Double
  | Array (Vector Reference)


newtype Reference = Reference (IORef Value)


instance Interval OnSyntax where
  start :: Num a => OnSyntax -> a
  start (BeforeExpression expression) = start expression
  start (AfterExpression expression) = start expression
  start (BeforeStatement statement) = start statement
  start (AfterStatement statement) = start statement
  start (BeforeDeclaration declaration) = start declaration
  start (AfterDeclaration declaration) = start declaration

  end :: Num a => OnSyntax -> a
  end (BeforeExpression expression) = end expression
  end (AfterExpression expression) = end expression
  end (BeforeStatement statement) = end statement
  end (AfterStatement statement) = end statement
  end (BeforeDeclaration declaration) = end declaration
  end (AfterDeclaration declaration) = end declaration


instance Applicative Evaluator where
  pure :: a -> Evaluator a
  pure x = Evaluator (\state -> pure (Done x, state))


  liftA2 :: (a -> b -> c) -> Evaluator a -> Evaluator b -> Evaluator c
  liftA2 f mx my = Evaluator $ \state -> do
    (result, state') <- runEvaluatorStep mx state

    case result of
      Done x -> runEvaluatorStep (f x <$> my) state'
      Step onSyntax mx -> pure (Step onSyntax (liftA2 f mx my), state')
      Error error -> pure (Error error, state')


instance Monad Evaluator where
  (>>=) :: Evaluator a -> (a -> Evaluator b) -> Evaluator b
  mx >>= f = Evaluator $ \state -> do
    (result, state') <- runEvaluatorStep mx state

    case result of
      Done x -> runEvaluatorStep (f x) state'
      Step onSyntax mx -> pure (Step onSyntax (f =<< mx), state')
      Error error -> pure (Error error, state')


instance MonadIO Evaluator where
  liftIO :: IO a -> Evaluator a
  liftIO mx = Evaluator $ \state -> do
    x <- mx
    pure (Done x, state)


instance MonadFail Evaluator where
  fail :: String -> Evaluator a
  fail message = liftIO (fail message)


makePredefinedState :: MonadIO m => m State
makePredefinedState = liftIO $ do
  true <- newReference (Bool True)
  false <- newReference (Bool False)
  unit <- newReference Unit
  let functions = [("toInt", BuiltinToInt), ("toFloat", BuiltinToFloat)]
  let variables = [("true", true), ("false", false), ("unit", unit)]
  pure [Frame 0 functions variables]


runEvaluatorStep :: MonadIO m => Evaluator a -> State -> m (Result a, State)
runEvaluatorStep (Evaluator f) state = liftIO (f state)


runEvaluator :: MonadIO m => Evaluator a -> State -> m (Either Error a, State)
runEvaluator mx state = do
  (result, state') <- runEvaluatorStep mx state

  case result of
    Done x -> pure (Right x, state')
    Step _ mx -> runEvaluator mx state'
    Error error -> pure (Left error, state')


newReference :: MonadIO m => Value -> m Reference
newReference x = liftIO $ do
  ref <- newIORef x
  pure (Reference ref)


readReference :: MonadIO m => Reference -> m Value
readReference (Reference ref) = liftIO (readIORef ref)


writeReference :: MonadIO m => Reference -> Value -> m ()
writeReference (Reference ref) v = liftIO (writeIORef ref v)


cloneReference :: MonadIO m => Reference -> m Reference
cloneReference r = do
  v <- readReference r

  case v of
    Unit -> newReference Unit
    Bool x -> newReference (Bool x)
    Int x -> newReference (Int x)
    Float x -> newReference (Float x)

    Array rs -> do
      rs' <- Vector.forM rs cloneReference
      newReference (Array rs')


compareReferences :: MonadIO m => Reference -> Reference -> m Bool
compareReferences r1 r2 = liftIO $ do
  v1 <- readReference r1
  v2 <- readReference r2
  compareValues v1 v2


compareValues :: MonadIO m => Value -> Value -> m Bool
compareValues v1 v2 = case (v1, v2) of
  (Unit, Unit) -> pure True
  (Bool x, Bool y) -> pure (x == y)
  (Int x, Int y) -> pure (x == y)
  (Float x, Float y) -> pure (x == y)

  (Array rs1, Array rs2) -> do
    let n1 = Vector.length rs1
    let n2 = Vector.length rs2

    if n1 /= n2 then
      pure False
    else
      go n1 0

    where
      go n i | i >= n = pure True

      go n i = do
        x <- readReference (rs1 ! i)
        y <- readReference (rs2 ! i)
        z <- x `compareValues` y

        if z then
          go n (i + 1)
        else
          pure False

  (_, _) -> pure False


defineFunction :: String -> Function -> Evaluator ()
defineFunction name function = Evaluator $ \case
  [] ->
    pure (Done (), [Frame 0 [(name, function)] []])

  frame : parents ->
    pure (Done (), frame {functions = (name, function) : functions frame} : parents)


lookupFunction :: String -> Evaluator (Maybe (Function, Int))
lookupFunction name = Evaluator (\state -> pure (Done (go 0 state), state))
  where
    go _ [] = Nothing

    go depth (Frame {offset, functions} : parents) = case lookup name functions of
      Just function -> Just (function, depth)
      Nothing -> go (depth + max 1 offset) (drop (offset - 1) parents)


defineVariable :: String -> Reference -> Evaluator ()
defineVariable name r = Evaluator $ \case
  [] ->
    pure (Done (), [Frame 0 [] [(name, r)]])

  frame : parents ->
    pure (Done (), frame {variables = (name, r) : variables frame} : parents)


lookupVariable :: String -> Evaluator (Maybe (Reference, Int))
lookupVariable name = Evaluator (\state -> pure (Done (go 0 state), state))
  where
    go _ [] = Nothing

    go depth (Frame {offset, variables} : parents) = case lookup name variables of
      Just ref -> Just (ref, depth)
      Nothing -> go (depth + max 1 offset) (drop (offset - 1) parents)


withNewFrame :: Int -> Evaluator a -> Evaluator a
withNewFrame offset mx = do
  pushFrame
  x <- mx
  popFrame
  pure x
  where
    pushFrame = Evaluator (\state -> pure (Done (), Frame offset [] [] : state))
    popFrame = Evaluator (\state -> pure (Done (), tail state))


raise :: Error -> Evaluator a
raise error = Evaluator (\state -> pure (Error error, state))


yield :: OnSyntax -> Evaluator ()
yield onSyntax = Evaluator (\state -> pure (Step onSyntax (pure ()), state))


onDeclaration :: (Declaration -> Evaluator b) -> Declaration -> Evaluator b
onDeclaration f declaration = do
  yield (BeforeDeclaration declaration)
  x <- f declaration
  yield (AfterDeclaration declaration)
  pure x


onStatement :: (Statement -> Evaluator b) -> Statement -> Evaluator b
onStatement f statement = do
  yield (BeforeStatement statement)
  x <- f statement
  yield (AfterStatement statement)
  pure x


onExpression :: (Expression -> Evaluator b) -> Expression -> Evaluator b
onExpression f expression = do
  yield (BeforeExpression expression)
  x <- f expression
  yield (AfterExpression expression)
  pure x
