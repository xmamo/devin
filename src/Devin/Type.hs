{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}

module Devin.Type (
  Type (..),
  (<:)
) where

import Data.Data

import Devin.Display


data Type
  = Unknown
  | Any
  | Unit
  | Bool
  | Int
  | Float
  | Array Type
  | Placeholder String
  deriving (Show, Read, Data)


(<:) :: Type -> Type -> Bool
Unknown <: _ = True
_ <: Unknown = True
_ <: Any = True
Unit <: Unit = True
Bool <: Bool = True
Int <: Int = True
Float <: Float = True
Array t1 <: Array t2 = t1 <: t2
Placeholder name1 <: Placeholder name2 = name1 == name2
_ <: _ = False


instance Display Type where
  displays :: Type -> ShowS
  displays Unknown = showChar '?'
  displays Any = showChar '∗'
  displays Unit = showString "Unit"
  displays Bool = showString "Bool"
  displays Int = showString "Int"
  displays Float = showString "Float"
  displays (Array t) = showChar '[' . displays t . showChar ']'
  displays (Placeholder name) = showString name
