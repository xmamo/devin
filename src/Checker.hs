module Checker (
  Checker (..),
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
import Error (Error)
import Environment (Environment)
import qualified Span
import Type (Type)


newtype Checker a where
  Checker :: (Environment -> (a, Environment, [Error])) -> Checker a
  deriving (Functor)


instance Applicative Checker where
  pure x = Checker \environment -> (x, environment, [])

  Checker check1 <*> Checker check2 = Checker \environment ->
    let (f, environment', errors1) = check1 environment
        (x, environment'', errors2) = check2 environment'
     in (f x, environment'', errors2 ++ errors1)


instance Monad Checker where
  Checker check1 >>= f = Checker \environment ->
    let (x1, environment', errors1) = check1 environment
        Checker check2 = f x1
        (x2, environment'', errors2) = check2 environment'
     in (x2, environment'', errors2 ++ errors1)


run :: Checker a -> Environment -> (a, Environment, [Error])
run (Checker check) environment =
  let (x, environment', errors) = check environment
   in (x, environment', sortOn Span.start errors)


getTypes :: Checker (Map Text Type)
getTypes = Checker \environment -> (environment.types, environment, [])


getVariables :: Checker (Map Text Type)
getVariables = Checker \environment -> (environment.variables, environment, [])


getFunctions :: Checker [Map Text [([Type], Type, CallTarget)]]
getFunctions = Checker \environment -> (environment.functions, environment, [])


setTypes :: Map Text Type -> Checker ()
setTypes types = Checker \environment -> ((), environment{types}, [])


setVariables :: Map Text Type -> Checker ()
setVariables variables = Checker \environment -> ((), environment{variables}, [])


setFunctions :: [Map Text [([Type], Type, CallTarget)]] -> Checker ()
setFunctions functions = Checker \environment -> ((), environment{functions}, [])


updateTypes :: (Map Text Type -> Map Text Type) -> Checker ()
updateTypes f = setTypes . f =<< getTypes


updateVariables :: (Map Text Type -> Map Text Type) -> Checker ()
updateVariables f = setVariables . f =<< getVariables


updateFunctions :: ([Map Text [([Type], Type, CallTarget)]] -> [Map Text [([Type], Type, CallTarget)]]) -> Checker ()
updateFunctions f = setFunctions . f =<< getFunctions


report :: Error -> Checker ()
report error = Checker \environment -> ((), environment, [error])


scoped :: Checker a -> Checker a
scoped (Checker check) = Checker \environment ->
  let (x, _, errors) = check environment
   in (x, environment, errors)
