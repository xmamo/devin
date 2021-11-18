module Devin.CallTarget (CallTarget (..)) where


data CallTarget where
  Undefined :: CallTarget
  IntToInt :: CallTarget
  FloatToInt :: CallTarget
  IntToFloat :: CallTarget
  FloatToFloat :: CallTarget
  UserDefined :: {position :: Int} -> CallTarget
  deriving (Eq, Show, Read)
