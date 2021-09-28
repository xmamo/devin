module Typer (
  Typer (..),
  run,
  getTypes,
  getVariables,
  getFunctions,
  setTypes,
  setVariables,
  setFunctions,
  updateTypes,
  updateFunctions,
  updateVariables,
  report,
  scoped
) where

import Data.List

import Data.Text (Text)

import Data.Map (Map)

import CallTarget (CallTarget)
import qualified Span
import Type (Type)
import Typer.Environment (Environment)
import Typer.Error (Error)


newtype Typer a = Typer (Environment -> (a, Environment, [Error]))
  deriving Functor


instance Applicative Typer where
  pure a = Typer \environment -> (a, environment, [])

  Typer check1 <*> Typer check2 = Typer \environment ->
    let (f, environment', errors1) = check1 environment
        (a, environment'', errors2) = check2 environment'
     in (f a, environment'', errors2 ++ errors1)


instance Monad Typer where
  Typer check1 >>= f = Typer \environment ->
    let (x1, environment', errors1) = check1 environment
        Typer check2 = f x1
        (x2, environment'', errors2) = check2 environment'
     in (x2, environment'', errors2 ++ errors1)


run :: Typer a -> Environment -> (a, Environment, [Error])
run (Typer check) environment =
  let (a, environment', errors) = check environment
   in (a, environment', sortOn Span.start errors)


getTypes :: Typer (Map Text Type)
getTypes = Typer \environment -> (environment.types, environment, [])


getVariables :: Typer (Map Text Type)
getVariables = Typer \environment -> (environment.variables, environment, [])


getFunctions :: Typer [Map Text [([Type], Type, CallTarget)]]
getFunctions = Typer \environment -> (environment.functions, environment, [])


setTypes :: Map Text Type -> Typer ()
setTypes types = Typer \environment -> ((), environment{types}, [])


setVariables :: Map Text Type -> Typer ()
setVariables variables = Typer \environment -> ((), environment{variables}, [])


setFunctions :: [Map Text [([Type], Type, CallTarget)]] -> Typer ()
setFunctions functions = Typer \environment -> ((), environment{functions}, [])


updateTypes :: (Map Text Type -> Map Text Type) -> Typer ()
updateTypes f = setTypes . f =<< getTypes


updateVariables :: (Map Text Type -> Map Text Type) -> Typer ()
updateVariables f = setVariables . f =<< getVariables


updateFunctions :: ([Map Text [([Type], Type, CallTarget)]] -> [Map Text [([Type], Type, CallTarget)]]) -> Typer ()
updateFunctions f = setFunctions . f =<< getFunctions


report :: Error -> Typer ()
report error = Typer \environment -> ((), environment, [error])


scoped :: Typer a -> Typer a
scoped (Typer check) = Typer \environment ->
  let (a, _, errors) = check environment
   in (a, environment, errors)
