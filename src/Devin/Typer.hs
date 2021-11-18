module Devin.Typer (
  Typer,
  runTyper,
  getDepth,
  getTypes,
  getVariables,
  getFunctions,
  defineType,
  defineVariable,
  defineFunction,
  push,
  report
) where

import Data.Functor.Classes
import Data.List

import Data.Text (Text)

import Data.Map (Map, (!?))
import qualified Data.Map as Map

import Devin.CallTarget (CallTarget)
import Devin.Range
import Devin.Syntax
import Devin.Type
import Devin.Typer.Environment
import Devin.Typer.Error


newtype Typer a = Typer (Environment -> (a, Environment, [Error]))
  deriving Functor


instance Applicative Typer where
  pure x = Typer $ \environment -> (x, environment, [])

  Typer check1 <*> Typer check2 = Typer $ \environment -> do
    let (f, environment', errors1) = check1 environment
    let (x, environment'', errors2) = check2 environment'
    (f x, environment'', errors2 ++ errors1)


instance Monad Typer where
  Typer check1 >>= f = Typer $ \environment -> do
    let (x1, environment', errors1) = check1 environment
    let Typer check2 = f x1
    let (x2, environment'', errors2) = check2 environment'
    (x2, environment'', errors2 ++ errors1)


runTyper :: Typer a -> Environment -> (a, Environment, [Error])
runTyper (Typer check) environment = do
  let (a, environment', errors) = check environment
  (a, environment', sortOn start errors)


getDepth :: Num a => Typer a
getDepth = Typer $ \environment ->
  (fromIntegral environment.depth, environment, [])


getTypes :: Typer (Map Text Type)
getTypes = Typer $ \environment ->
  (environment.types, environment, [])


getVariables :: Typer (Map Text Type)
getVariables = Typer $ \environment ->
  (environment.variables, environment, [])


getFunctions :: Typer [Map Text [([Type], Type, CallTarget)]]
getFunctions = Typer $ \environment ->
  (environment.functions, environment, [])


defineType :: Identifier -> Typer ()
defineType typeId = Typer $ \environment -> do
  let types' = Map.insert typeId.name typeId.t environment.types
  ((), environment{types = types'}, [])


defineVariable :: Identifier -> Typer ()
defineVariable variableId = Typer $ \environment -> do
  let variables' = Map.insert variableId.name variableId.t environment.variables
  ((), environment{variables = variables'}, [])


defineFunction :: Identifier -> CallTarget -> Typer Bool
defineFunction functionId callTarget = Typer $ \environment -> do
  let parameterTypes = functionId.t.parameterTypes
  let returnType = functionId.t.returnType
  let current : parents = environment.functions

  case current !? functionId.name of
    Just infos | any (\i -> liftEq areCompatible parameterTypes i._1) infos ->
      (False, environment, [FunctionRedefinition functionId parameterTypes])

    Just infos -> do
      let infos' = (parameterTypes, returnType, callTarget) : infos
      let functions' = Map.insert functionId.name infos' current : parents
      (True, environment{functions = functions'}, [])

    Nothing -> do
      let infos' = [(parameterTypes, returnType, callTarget)]
      let functions' = Map.insert functionId.name infos' current : parents
      (True, environment{functions = functions'}, [])


push :: Typer a -> Typer a
push (Typer check) = Typer $ \environment -> do
  let (a, _, errors) = check environment{depth = environment.depth + 1}
  (a, environment, errors)


report :: Error -> Typer ()
report error = Typer $ \environment -> ((), environment, [error])
