{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}

module Devin.Type (
  Type (..),
  (<:),
  merge
) where

import Data.Data
import Data.Maybe

import Devin.Display


data Type
  = Unknown
  | Unit
  | Bool
  | Int
  | Float
  | Array Type
  | Placeholder String
  deriving (Show, Read, Data)


(<:) :: Type -> Type -> Bool
t1 <: t2 = isJust (merge t1 t2)


merge :: Type -> Type -> Maybe Type
merge Unknown _ = Just Unknown
merge _ Unknown = Just Unknown
merge Unit Unit = Just Unit
merge Bool Bool = Just Bool
merge Int Int = Just Int
merge Float Float = Just Float
merge (Array t1) (Array t2) = Array <$> merge t1 t2
merge (Placeholder n1) (Placeholder n2) | n1 == n2 = Just (Placeholder n1)
merge _ _ = Nothing


instance Display Type where
  displays :: Type -> ShowS
  displays Unknown = showChar '?'
  displays Unit = showString "Unit"
  displays Bool = showString "Bool"
  displays Int = showString "Int"
  displays Float = showString "Float"
  displays (Array t) = showChar '[' . displays t . showChar ']'
  displays (Placeholder name) = showString name
