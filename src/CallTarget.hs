module CallTarget (CallTarget (..)) where

import qualified Syntax


data CallTarget where
  BuiltinInt :: CallTarget
  BuiltinFloat :: CallTarget
  UserDefined :: {parameters :: [Syntax.Identifier], body :: Syntax.Statement} -> CallTarget
  Error :: CallTarget
  deriving (Eq, Show, Read)
