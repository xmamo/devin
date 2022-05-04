{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Typer (
  Typer (..),
  Environment,
  Scope (..),
  predefinedEnvironment,
  defineType,
  lookupType,
  defineFunctionSignature,
  lookupFunctionSignature,
  defineVariableType,
  lookupVariableType,
  withNewScope,
  report
) where

import Control.Applicative
import Data.Data

import Devin.Error
import Devin.Type


newtype Typer a = Typer {runTyper :: Environment -> (a, Environment, [Error])}
  deriving Functor


type Environment = [Scope]


data Scope = Scope {
  types :: [(String, Type)],
  functions :: [(String, ([Type], Type))],
  variables :: [(String, Type)]
} deriving (Show, Read, Data)


instance Applicative Typer where
  pure :: a -> Typer a
  pure x = Typer (\environment -> (x, environment, []))


  liftA2 :: (a -> b -> c) -> Typer a -> Typer b -> Typer c
  liftA2 f mx my = Typer $ \environment ->
    let (x, environment', errors1) = runTyper mx environment
        (y, environment'', errors2) = runTyper my environment'
     in (f x y, environment'', errors1 ++ errors2)


instance Monad Typer where
  (>>=) :: Typer a -> (a -> Typer b) -> Typer b
  mx >>= f = Typer $ \environment ->
    let (x, environment', errors1) = runTyper mx environment
        (y, environment'', errors2) = runTyper (f x) environment'
     in (y, environment'', errors1 ++ errors2)


predefinedEnvironment :: Environment
predefinedEnvironment =
  let types = [("Unit", Unit), ("Bool", Bool), ("Int", Int), ("Float", Float)]
      functions = [("toInt", ([Float], Int)), ("toFloat", ([Int], Float))]
      variables = [("true", Bool), ("false", Bool), ("unit", Unit)]
   in [Scope types functions variables]


defineType :: String -> Type -> Typer Type
defineType name t = Typer $ \case
  [] ->
    (t, [Scope [(name, t)] [] []], [])

  scope : parents ->
    (t, scope {types = (name, t) : types scope} : parents, [])


lookupType :: String -> Typer (Maybe (Type, Int))
lookupType name = Typer (\environment -> (go 0 environment, environment, []))
  where
    go _ [] = Nothing

    go depth (Scope {types} : parents) = case lookup name types of
      Just t -> Just (t, depth)
      Nothing -> go (depth + 1) parents


defineFunctionSignature :: String -> ([Type], Type) -> Typer ()
defineFunctionSignature name signature = Typer $ \case
  [] -> ((), [Scope [] [(name, signature)] []], [])
  scope : parents -> ((), scope {functions = (name, signature) : functions scope} : parents, [])


lookupFunctionSignature :: String -> Typer (Maybe (([Type], Type), Int))
lookupFunctionSignature name = Typer (\environment -> (go 0 environment, environment, []))
  where
    go _ [] = Nothing

    go depth (Scope {functions} : parents) = case lookup name functions of
      Just signature -> Just (signature, depth)
      Nothing -> go (depth + 1) parents


defineVariableType :: String -> Type -> Typer ()
defineVariableType name t = Typer $ \case
  [] -> ((), [Scope [] [] [(name, t)]], [])
  scope : parents -> ((), scope {variables = (name, t) : variables scope} : parents, [])


lookupVariableType :: String -> Typer (Maybe (Type, Int))
lookupVariableType name = Typer (\environment -> (go 0 environment, environment, []))
  where
    go _ [] = Nothing

    go depth (Scope {variables} : parents) = case lookup name variables of
      Just t -> Just (t, depth)
      Nothing -> go (depth + 1) parents


withNewScope :: Typer a -> Typer a
withNewScope mx = Typer $ \environment ->
  let (x, environment', errors) = runTyper mx (Scope [] [] [] : environment)
   in (x, tail environment', errors)


report :: Error -> Typer ()
report error = Typer (\environment -> ((), environment, [error]))
