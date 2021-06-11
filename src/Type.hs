module Type (
  Type (..),
  Pass (..),
  Environment (..),
  Error (..),
  isError,
  description,
  span,
  start,
  end,
  checkDeclarations,
  checkDeclaration,
  checkStatement,
  checkExpression,
  defaultEnvironment
) where

import Prelude hiding (span)

import qualified Data.Map as Map

import Control.Monad.Trans.Writer

import qualified Syntax
import Type.Common
import qualified Type.Internal as Internal
import qualified Unicode


checkDeclarations :: Foldable t => Environment -> t (Syntax.Declaration ()) -> [Error]
checkDeclarations environment declarations = execWriter (Internal.checkDeclarations environment declarations)


checkDeclaration :: Pass -> Environment -> Syntax.Declaration () -> (Environment, [Error])
checkDeclaration pass environment declaration = runWriter (Internal.checkDeclaration pass environment declaration)


checkStatement :: Type -> Environment -> Syntax.Statement () -> (Bool, Environment, [Error])
checkStatement expectedType environment statement =
  let ((doesReturn, environment'), errors) = runWriter (Internal.checkStatement expectedType environment statement)
   in (doesReturn, environment', errors)


checkExpression :: Environment -> Syntax.Expression () -> (Type, Environment, [Error])
checkExpression environment expression =
  let ((t, environment'), errors) = runWriter (Internal.checkExpression environment expression)
   in (t, environment', errors)


defaultEnvironment :: Environment
defaultEnvironment = Environment types variables functions
  where
    types = Map.fromList
      [
        (Unicode.collate "Unit", Unit),
        (Unicode.collate "Boolean", Boolean),
        (Unicode.collate "Integer", Integer),
        (Unicode.collate "Rational", Rational)
      ]

    variables = Map.fromList
      [
        (Unicode.collate "unit", Unit),
        (Unicode.collate "true", Boolean),
        (Unicode.collate "false", Boolean)
      ]

    functions =
      [
        Map.empty,

        Map.fromList [
          (Unicode.collate "integer", [([Integer], Integer), ([Rational], Integer)]),
          (Unicode.collate "rational", [([Integer], Rational), ([Rational], Rational)])
        ]
      ]
