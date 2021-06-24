module Checker (
  Checker (..),
  run,
  getTypes,
  getVariables,
  getFunctions,
  updateTypes,
  updateFunctions,
  updateVariables,
  report,
  scoped
) where

import Data.List
import Data.List.NonEmpty (NonEmpty)

import Data.Text (Text)

import Data.Map (Map)

import Error (Error)
import Environment (Environment)
import qualified Error
import Type (Type)


data Checker a where
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
   in (x, environment', sortOn Error.start errors)


getTypes :: Checker (Map Text Type)
getTypes = Checker \environment -> (environment.types, environment, [])


getVariables :: Checker (Map Text Type)
getVariables = Checker \environment -> (environment.variables, environment, [])


getFunctions :: Checker (NonEmpty (Map Text [([Type], Type)]))
getFunctions = Checker \environment -> (environment.functions, environment, [])


updateTypes :: (Map Text Type -> Map Text Type) -> Checker ()
updateTypes f = Checker \environment -> ((), environment{types = f environment.types}, [])


updateVariables :: (Map Text Type -> Map Text Type) -> Checker ()
updateVariables f = Checker \environment -> ((), environment{variables = f environment.variables}, [])


updateFunctions :: (NonEmpty (Map Text [([Type], Type)]) -> NonEmpty (Map Text [([Type], Type)])) -> Checker ()
updateFunctions f = Checker \environment -> ((), environment{functions = f environment.functions}, [])


report :: Error -> Checker ()
report error = Checker \environment -> ((), environment, [error])


scoped :: Checker a -> Checker a
scoped (Checker check) = Checker \environment ->
  let (x, _, errors) = check environment
   in (x, environment, errors)
