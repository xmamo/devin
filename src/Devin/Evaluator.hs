{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Evaluator (
  Evaluator (..),
  Result (..),
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
  cloneValue,
  compareValues,
  defineFunction,
  lookupFunction,
  defineVariable,
  lookupVariable,
  withNewFrame,
  debug,
  raise
) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Data
import Data.IORef
import Data.Int

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

import Devin.Error
import Devin.Syntax


newtype Evaluator a = Evaluator (State -> IO (Result a, State))
  deriving Functor


data Result a
  = Done a
  | Debug Statement (Evaluator a)
  | Error Error
  deriving Functor


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


instance Applicative Evaluator where
  pure :: a -> Evaluator a
  pure x = Evaluator (\state -> pure (Done x, state))


  liftA2 :: (a -> b -> c) -> Evaluator a -> Evaluator b -> Evaluator c
  liftA2 f mx my = Evaluator $ \state -> do
    (result, state') <- runEvaluatorStep mx state

    case result of
      Done x -> runEvaluatorStep (f x <$> my) state'
      Debug statement mx -> pure (Debug statement (liftA2 f mx my), state')
      Error error -> pure (Error error, state')


instance Monad Evaluator where
  (>>=) :: Evaluator a -> (a -> Evaluator b) -> Evaluator b
  mx >>= f = Evaluator $ \state -> do
    (result, state') <- runEvaluatorStep mx state

    case result of
      Done x -> runEvaluatorStep (f x) state'
      Debug statement mx -> pure (Debug statement (f =<< mx), state')
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
    Debug _ mx -> runEvaluator mx state'
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
  v' <- cloneValue v
  newReference v'


compareReferences :: MonadIO m => Reference -> Reference -> m Bool
compareReferences r1 r2 = liftIO $ do
  v1 <- readReference r1
  v2 <- readReference r2
  compareValues v1 v2


cloneValue :: MonadIO m => Value -> m Value
cloneValue = \case
  Unit -> pure Unit
  Bool x -> pure (Bool x)
  Int x -> pure (Int x)
  Float x -> pure (Float x)

  Array rs -> do
    rs' <- Vector.forM rs cloneReference
    pure (Array rs')


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


debug :: Statement -> Evaluator ()
debug statement = Evaluator (\state -> pure (Debug statement (pure ()), state))


raise :: Error -> Evaluator a
raise error = Evaluator (\state -> pure (Error error, state))
