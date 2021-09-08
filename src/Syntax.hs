module Syntax (
  Internal.Node (..),
  Internal.Declaration (..),
  Internal.Statement (..),
  Internal.Expression (..),
  Internal.UnaryOperator (..),
  Internal.BinaryOperator (..),
  Internal.AssignOperator (..),
  Internal.Identifier (..),
  Internal.Token (..),
  Internal.Comment (..),
  Internal.doesReturn,
  Internal.hasSideEffects,
  Internal.comparePrecedence
) where

import qualified Syntax.Internal as Internal
