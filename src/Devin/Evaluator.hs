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
  runEvaluatorStep,
  runEvaluator,
  makePredefinedState,
  cloneVal,
  compareVals,
  newRef,
  readRef,
  writeRef,
  cloneRef,
  compareRefs,
  defineFun,
  lookupFun,
  defineVar,
  lookupVar,
  withNewFrame,
  breakpoint,
  raise
) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Data
import Data.IORef
import Data.Int

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

import Control.Monad.Extra

import Devin.Error
import Devin.Syntax


newtype Evaluator a = Evaluator (State -> IO (Result a, State))
  deriving Functor


data Result a
  = Done a
  | Breakpoint Statement (Evaluator a)
  | Error Error
  deriving Functor


type State = [Frame]


data Frame = Frame {
  offset :: Int,  -- Access link / static link
  funs :: [(String, Function)],
  vars :: [(String, Reference)]
}


data Function
  = UserDefined Definition
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
      Breakpoint statement mx -> pure (Breakpoint statement (liftA2 f mx my), state')
      Error error -> pure (Error error, state')


instance Monad Evaluator where
  (>>=) :: Evaluator a -> (a -> Evaluator b) -> Evaluator b
  mx >>= f = Evaluator $ \state -> do
    (result, state') <- runEvaluatorStep mx state

    case result of
      Done x -> runEvaluatorStep (f x) state'
      Breakpoint statement mx -> pure (Breakpoint statement (f =<< mx), state')
      Error error -> pure (Error error, state')


instance MonadIO Evaluator where
  liftIO :: IO a -> Evaluator a
  liftIO mx = Evaluator $ \state -> do
    x <- mx
    pure (Done x, state)


instance MonadFail Evaluator where
  fail :: String -> Evaluator a
  fail message = liftIO (fail message)


runEvaluatorStep :: MonadIO m => Evaluator a -> State -> m (Result a, State)
runEvaluatorStep (Evaluator f) state = liftIO (f state)


runEvaluator :: MonadIO m => Evaluator a -> State -> m (Either Error a, State)
runEvaluator mx state = do
  (result, state') <- runEvaluatorStep mx state

  case result of
    Done x -> pure (Right x, state')
    Breakpoint _ mx -> runEvaluator mx state'
    Error error -> pure (Left error, state')


makePredefinedState :: MonadIO m => m State
makePredefinedState = liftIO $ do
  trueR <- newRef (Bool True)
  falseR <- newRef (Bool False)
  unitR <- newRef Unit
  let funs = [("toInt", BuiltinToInt), ("toFloat", BuiltinToFloat)]
  let vars = [("true", trueR), ("false", falseR), ("unit", unitR)]
  pure [Frame 0 funs vars]


cloneVal :: MonadIO m => Value -> m Value
cloneVal = \case
  Unit -> pure Unit
  Bool x -> pure (Bool x)
  Int x -> pure (Int x)
  Float x -> pure (Float x)

  Array rs -> do
    rs' <- Vector.forM rs cloneRef
    pure (Array rs')


compareVals :: MonadIO m => Value -> Value -> m Bool
compareVals v1 v2 = case (v1, v2) of
  (Unit, Unit) -> pure True
  (Bool x, Bool y) -> pure (x == y)
  (Int x, Int y) -> pure (x == y)
  (Float x, Float y) -> pure (x == y)

  (Array rs1, Array rs2) -> do
    let n1 = Vector.length rs1
    let n2 = Vector.length rs2
    if n1 /= n2 then pure False else go n1 0

    where
      go n i | i >= n = pure True

      go n i = do
        x <- readRef (rs1 ! i)
        y <- readRef (rs2 ! i)
        compareVals x y &&^ go n (i + 1)

  (_, _) -> pure False


newRef :: MonadIO m => Value -> m Reference
newRef v = liftIO $ do
  ref <- newIORef v
  pure (Reference ref)


readRef :: MonadIO m => Reference -> m Value
readRef (Reference ref) = liftIO (readIORef ref)


writeRef :: MonadIO m => Reference -> Value -> m ()
writeRef (Reference ref) v = liftIO (writeIORef ref v)


cloneRef :: MonadIO m => Reference -> m Reference
cloneRef r = do
  v <- readRef r
  v' <- cloneVal v
  newRef v'


compareRefs :: MonadIO m => Reference -> Reference -> m Bool
compareRefs r1 r2 = do
  v1 <- readRef r1
  v2 <- readRef r2
  compareVals v1 v2


defineFun :: String -> Function -> Evaluator ()
defineFun name fun = Evaluator $ \case
  [] -> pure (Done (), [Frame 0 [(name, fun)] []])

  frame : parents -> do
    let funs' = (name, fun) : funs frame
    pure (Done (), frame {funs = funs'} : parents)


lookupFun :: String -> Evaluator (Maybe (Function, Int))
lookupFun name = Evaluator (\state -> pure (Done (go 0 state), state))
  where
    go _ [] = Nothing

    go depth (Frame {offset, funs} : parents) = case lookup name funs of
      Just fun -> Just (fun, depth)
      Nothing -> go (depth + max offset 1) (drop (offset - 1) parents)


defineVar :: String -> Reference -> Evaluator ()
defineVar name r = Evaluator $ \case
  [] -> pure (Done (), [Frame 0 [] [(name, r)]])

  frame : parents -> do
    let vars' = (name, r) : vars frame
    pure (Done (), frame {vars = vars'} : parents)


lookupVar :: String -> Evaluator (Maybe (Reference, Int))
lookupVar name = Evaluator (\state -> pure (Done (go 0 state), state))
  where
    go _ [] = Nothing

    go depth (Frame {offset, vars} : parents) = case lookup name vars of
      Just ref -> Just (ref, depth)
      Nothing -> go (depth + max offset 1) (drop (offset - 1) parents)


withNewFrame :: Int -> Evaluator a -> Evaluator a
withNewFrame offset mx = do
  pushFrame
  x <- mx
  popFrame
  pure x
  where
    pushFrame = Evaluator (\state -> pure (Done (), Frame offset [] [] : state))
    popFrame = Evaluator (\state -> pure (Done (), tail state))


breakpoint :: Statement -> Evaluator ()
breakpoint statement = Evaluator $ \state ->
  pure (Breakpoint statement (pure ()), state)


raise :: Error -> Evaluator a
raise error = Evaluator $ \state ->
  pure (Error error, state)
