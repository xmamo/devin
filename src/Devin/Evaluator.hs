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
  Cell,
  runEvaluatorStep,
  runEvaluator,
  makePredefinedState,
  getType,
  cloneVal,
  compareVals,
  newCell,
  readCell,
  writeCell,
  cloneCell,
  compareCells,
  defineFun,
  lookupFun,
  defineVar,
  lookupVar,
  withNewFrame,
  yield,
  raise
) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Data
import Data.Int
import Data.IORef

import qualified Data.Vector as Vector
import Data.Vector (Vector, (!))

import Data.Foldable.Extra (allM)

import Devin.Error
import Devin.Syntax
import qualified Devin.Type as Type
import Devin.Type (Type, (<:))


newtype Evaluator a = Evaluator (State -> IO (Result a, State))
  deriving Functor


data Result a
  = Done a
  | Yield Statement (Evaluator a)
  | Error Error
  deriving Functor


type State = [Frame]


data Frame = Frame {
  label :: Maybe String,
  poffset :: Int,  -- Static link
  funs :: [(String, Function)],
  vars :: [(String, Cell)]
}


data Function
  = UserDefined Definition
  | BuiltinNot
  | BuiltinLen
  | BuiltinToInt
  | BuiltinToFloat
  deriving (Eq, Show, Read, Data)


data Value
  = Unit
  | Bool Bool
  | Int Int64
  | Float Double
  | Array (Vector Cell)


newtype Cell = Cell (IORef Value)


instance Applicative Evaluator where
  pure :: a -> Evaluator a
  pure x = Evaluator (\state -> pure (Done x, state))


  liftA2 :: (a -> b -> c) -> Evaluator a -> Evaluator b -> Evaluator c
  liftA2 f mx my = Evaluator $ \state -> do
    (result, state') <- runEvaluatorStep mx state

    case result of
      Done x -> runEvaluatorStep (f x <$> my) state'
      Yield statement mx -> pure (Yield statement (liftA2 f mx my), state')
      Error error -> pure (Error error, state')


instance Monad Evaluator where
  (>>=) :: Evaluator a -> (a -> Evaluator b) -> Evaluator b
  mx >>= f = Evaluator $ \state -> do
    (result, state') <- runEvaluatorStep mx state

    case result of
      Done x -> runEvaluatorStep (f x) state'
      Yield statement mx -> pure (Yield statement (f =<< mx), state')
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
    Yield _ mx -> runEvaluator mx state'
    Error error -> pure (Left error, state')


makePredefinedState :: MonadIO m => m State
makePredefinedState = liftIO $ do
  let f1 = ("not", BuiltinNot)
  let f2 = ("len", BuiltinLen)
  let f3 = ("toInt", BuiltinToInt)
  let f4 = ("toFloat", BuiltinToFloat)

  v1 <- (,) "true" <$> newCell (Bool True)
  v2 <- (,) "false" <$> newCell (Bool False)
  v3 <- (,) "unit" <$> newCell Unit

  pure [Frame Nothing 0 [f4, f3, f2, f1] [v3, v2, v1]]


getType :: MonadIO m => Value -> m Type
getType = \case
  Unit -> pure Type.Unit
  Bool _ -> pure Type.Bool
  Int _ -> pure Type.Int
  Float _ -> pure Type.Float

  Array cells | Vector.null cells -> pure (Type.Array Type.Unknown)

  Array cells -> do
    cell <- readCell (Vector.head cells)
    t <- getType cell

    let f Type.Unknown = False
        f t' = t' <: t

    ok <- allM (\cell -> f <$> (getType =<< readCell cell)) cells
    pure (Type.Array (if ok then t else Type.Unknown))


cloneVal :: MonadIO m => Value -> m Value
cloneVal = \case
  Unit -> pure Unit
  Bool x -> pure (Bool x)
  Int x -> pure (Int x)
  Float x -> pure (Float x)

  Array cells -> do
    cells' <- Vector.forM cells cloneCell
    pure (Array cells')


compareVals :: MonadIO m => Value -> Value -> m (Either (Type, Type) Ordering)
compareVals v1 v2 = case (v1, v2) of
  (Unit, Unit) -> pure (Right EQ)
  (Bool x, Bool y) -> pure (Right (compare x y))
  (Int x, Int y) -> pure (Right (compare x y))
  (Float x, Float y) -> pure (Right (compare x y))

  (Array cells1, Array cells2) -> go (Vector.length cells1) (Vector.length cells2) 0
    where
      go n1 n2 i | i >= n1 =
        pure (Right (if i == n2 then EQ else LT))

      go n1 n2 i | i >= n2 =
        pure (Right (if i == n1 then EQ else GT))

      go n1 n2 i = do
        val1 <- readCell (cells1 ! i)
        val2 <- readCell (cells2 ! i)

        compareVals val1 val2 >>= \case
          Right EQ -> go n1 n2 (i + 1)
          Right ordering -> pure (Right ordering)
          Left (t1, t2) -> pure (Left (t1, t2))

  (_, _) -> do
    t1 <- getType v1
    t2 <- getType v2
    pure (Left (t1, t2))


newCell :: MonadIO m => Value -> m Cell
newCell val = liftIO $ do
  ref <- newIORef val
  pure (Cell ref)


readCell :: MonadIO m => Cell -> m Value
readCell (Cell ref) = liftIO (readIORef ref)


writeCell :: MonadIO m => Cell -> Value -> m Cell
writeCell (Cell ref) val = liftIO $ do
  writeIORef ref val
  pure (Cell ref)


cloneCell :: MonadIO m => Cell -> m Cell
cloneCell cell = do
  val <- readCell cell
  val' <- cloneVal val
  newCell val'


compareCells :: MonadIO m => Cell -> Cell -> m (Either (Type, Type) Ordering)
compareCells cell1 cell2 = do
  val1 <- readCell cell1
  val2 <- readCell cell2
  compareVals val1 val2


defineFun :: String -> Function -> Evaluator ()
defineFun name fun = Evaluator $ \case
  [] -> pure (Done (), [Frame Nothing 0 [(name, fun)] []])

  frame : frames -> do
    let funs' = (name, fun) : funs frame
    pure (Done (), frame {funs = funs'} : frames)


lookupFun :: String -> Evaluator (Maybe (Function, Int))
lookupFun name = Evaluator (\state -> pure (Done (go 0 state), state))
  where
    go _ [] = Nothing

    go depth (Frame {poffset, funs} : frames) = case lookup name funs of
      Just fun -> Just (fun, depth)
      Nothing -> go (depth + max 1 poffset) (drop (poffset - 1) frames)


defineVar :: String -> Cell -> Evaluator ()
defineVar name cell = Evaluator $ \case
  [] -> pure (Done (), [Frame Nothing 0 [] [(name, cell)]])

  frame : frames -> do
    let vars' = (name, cell) : vars frame
    pure (Done (), frame {vars = vars'} : frames)


lookupVar :: String -> Evaluator (Maybe (Cell, Int))
lookupVar name = Evaluator (\state -> pure (Done (go 0 state), state))
  where
    go _ [] = Nothing

    go depth (Frame {poffset, vars} : frames) = case lookup name vars of
      Just cell -> Just (cell, depth)
      Nothing -> go (depth + max 1 poffset) (drop (poffset - 1) frames)


withNewFrame :: Maybe String -> Int -> Evaluator a -> Evaluator a
withNewFrame label poffset mx = do
  pushFrame
  x <- mx
  popFrame
  pure x

  where
    pushFrame = Evaluator $ \state ->
      pure (Done (), Frame label poffset [] [] : state)

    popFrame = Evaluator $ \state ->
      pure (Done (), tail state)


yield :: Statement -> Evaluator ()
yield statement = Evaluator $ \state ->
  pure (Yield statement (pure ()), state)


raise :: Error -> Evaluator a
raise error = Evaluator $ \state ->
  pure (Error error, state)
