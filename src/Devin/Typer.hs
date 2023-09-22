{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Devin.Typer (
  Typer,
  Environment,
  Scope (..),
  predefinedEnv,
  runTyper,
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


newtype Typer a
  = Typer { unTyper :: Environment -> (a, Environment, [Error] -> [Error]) }
  deriving Functor


type Environment = [Scope]


data Scope = Scope {
  types :: [(String, Type)],
  funs :: [(String, ([Type], Type))],
  vars :: [(String, Type)]
} deriving (Show, Read, Data)


instance Applicative Typer where
  pure :: a -> Typer a
  pure x = Typer (\env -> (x, env, id))


  liftA2 :: (a -> b -> c) -> Typer a -> Typer b -> Typer c
  liftA2 f mx my = Typer $ \env ->
    let (x, env', errorDL1) = unTyper mx env
        (y, env'', errorDL2) = unTyper my env'
     in (f x y, env'', errorDL1 . errorDL2)


instance Monad Typer where
  (>>=) :: Typer a -> (a -> Typer b) -> Typer b
  mx >>= f = Typer $ \env ->
    let (x, env', errorDL1) = unTyper mx env
        (y, env'', errorDL2) = unTyper (f x) env'
     in (y, env'', errorDL1 . errorDL2)


predefinedEnv :: Environment
predefinedEnv =
  let t1 = ("Unit", Unit)
      t2 = ("Bool", Bool)
      t3 = ("Int", Int)
      t4 = ("Float", Float)

      f1 = ("not", ([Bool], Bool))
      f2 = ("len", ([Array Unknown], Int))
      f3 = ("intToFloat", ([Int], Float))
      f4 = ("floatToInt", ([Float], Int))

      v1 = ("true", Bool)
      v2 = ("false", Bool)
      v3 = ("unit", Unit)

   in [Scope [t4, t3, t2, t1] [f4, f3, f2, f1] [v3, v2, v1]]


runTyper :: Typer a -> Environment -> (a, Environment, [Error])
runTyper mx env =
  let (x, env', errorDL) = unTyper mx env
   in (x, env', errorDL [])


defineType :: String -> Type -> Typer Type
defineType name t = Typer $ \case
  [] -> (t, [Scope [(name, t)] [] []], id)

  scope : scopes ->
    let types' = (name, t) : types scope
     in (t, scope{types = types'} : scopes, id)


lookupType :: String -> Typer (Maybe (Type, Int))
lookupType name = Typer (\env -> (go 0 env, env, id))
  where
    go _ [] = Nothing

    go depth (Scope{types} : scopes) = case lookup name types of
      Just t -> Just (t, depth)
      Nothing -> go (depth + 1) scopes


defineFunSignature :: String -> ([Type], Type) -> Typer ()
defineFunSignature name signature = Typer $ \case
  [] -> ((), [Scope [] [(name, signature)] []], id)

  scope : scopes ->
    let funs' = (name, signature) : funs scope
     in ((), scope{funs = funs'} : scopes, id)


lookupFunSignature :: String -> Typer (Maybe (([Type], Type), Int))
lookupFunSignature name = Typer (\env -> (go 0 env, env, id))
  where
    go _ [] = Nothing

    go depth (Scope{funs} : scopes) = case lookup name funs of
      Just signature -> Just (signature, depth)
      Nothing -> go (depth + 1) scopes


defineVarType :: String -> Type -> Typer ()
defineVarType name t = Typer $ \case
  [] -> ((), [Scope [] [] [(name, t)]], id)

  scope : scopes ->
    let vars' = (name, t) : vars scope
     in ((), scope{vars = vars'} : scopes, id)


lookupVarType :: String -> Typer (Maybe (Type, Int))
lookupVarType name = Typer (\env -> (go 0 env, env, id))
  where
    go _ [] = Nothing

    go depth (Scope{vars} : scopes) = case lookup name vars of
      Just t -> Just (t, depth)
      Nothing -> go (depth + 1) scopes


withNewScope :: Typer a -> Typer a
withNewScope mx = Typer $ \env ->
  let (x, env', errors) = unTyper mx (Scope [] [] [] : env)
   in (x, tail env', errors)


report :: Error -> Typer ()
report error = Typer (\env -> ((), env, (error :)))
