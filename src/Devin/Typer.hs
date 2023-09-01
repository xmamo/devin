{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Typer (
  Typer (..),
  Environment,
  Scope (..),
  predefinedEnv,
  defineType,
  lookupType,
  defineFunSignature,
  lookupFunSignature,
  defineVarType,
  lookupVarType,
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
  funs :: [(String, ([Type], Type))],
  vars :: [(String, Type)]
} deriving (Show, Read, Data)


instance Applicative Typer where
  pure :: a -> Typer a
  pure x = Typer (\env -> (x, env, []))


  liftA2 :: (a -> b -> c) -> Typer a -> Typer b -> Typer c
  liftA2 f mx my = Typer $ \env ->
    let (x, env', errors1) = runTyper mx env
        (y, env'', errors2) = runTyper my env'
     in (f x y, env'', errors1 ++ errors2)


instance Monad Typer where
  (>>=) :: Typer a -> (a -> Typer b) -> Typer b
  mx >>= f = Typer $ \env ->
    let (x, env', errors1) = runTyper mx env
        (y, env'', errors2) = runTyper (f x) env'
     in (y, env'', errors1 ++ errors2)


predefinedEnv :: Environment
predefinedEnv =
  let types = [("Unit", Unit), ("Bool", Bool), ("Int", Int), ("Float", Float)]
      funs = [("toInt", ([Float], Int)), ("toFloat", ([Int], Float))]
      vars = [("true", Bool), ("false", Bool), ("unit", Unit)]
   in [Scope types funs vars]


defineType :: String -> Type -> Typer Type
defineType name t = Typer $ \case
  [] -> (t, [Scope [(name, t)] [] []], [])

  scope : scopes ->
    let types' = (name, t) : types scope
     in (t, scope {types = types'} : scopes, [])


lookupType :: String -> Typer (Maybe (Type, Int))
lookupType name = Typer (\env -> (go 0 env, env, []))
  where
    go _ [] = Nothing

    go depth (Scope {types} : scopes) = case lookup name types of
      Just t -> Just (t, depth)
      Nothing -> go (depth + 1) scopes


defineFunSignature :: String -> ([Type], Type) -> Typer ()
defineFunSignature name signature = Typer $ \case
  [] -> ((), [Scope [] [(name, signature)] []], [])

  scope : scopes ->
    let funs' = (name, signature) : funs scope
     in ((), scope {funs = funs'} : scopes, [])


lookupFunSignature :: String -> Typer (Maybe (([Type], Type), Int))
lookupFunSignature name = Typer (\env -> (go 0 env, env, []))
  where
    go _ [] = Nothing

    go depth (Scope {funs} : scopes) = case lookup name funs of
      Just signature -> Just (signature, depth)
      Nothing -> go (depth + 1) scopes


defineVarType :: String -> Type -> Typer ()
defineVarType name t = Typer $ \case
  [] -> ((), [Scope [] [] [(name, t)]], [])

  scope : scopes ->
    let vars' = (name, t) : vars scope
     in ((), scope {vars = vars'} : scopes, [])


lookupVarType :: String -> Typer (Maybe (Type, Int))
lookupVarType name = Typer (\env -> (go 0 env, env, []))
  where
    go _ [] = Nothing

    go depth (Scope {vars} : scopes) = case lookup name vars of
      Just t -> Just (t, depth)
      Nothing -> go (depth + 1) scopes


withNewScope :: Typer a -> Typer a
withNewScope mx = Typer $ \env ->
  let (x, env', errors) = runTyper mx (Scope [] [] [] : env)
   in (x, tail env', errors)


report :: Error -> Typer ()
report error = Typer (\env -> ((), env, [error]))
