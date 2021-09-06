module CallTarget (CallTarget (..)) where

import qualified Syntax


data CallTarget where
  Undefined :: CallTarget
  IntToInt :: CallTarget
  FloatToInt :: CallTarget
  IntToFloat :: CallTarget
  FloatToFloat :: CallTarget
  UserDefined :: {parameters :: [Syntax.Identifier], body :: Syntax.Statement} -> CallTarget
  deriving (Eq, Show, Read)
