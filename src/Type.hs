module Type (
  Type (..),
  Environment (..),
  Error (..),
  label,
  areCompatible,
  description,
  span,
  start,
  end,
  defaultEnvironment,
  checkDeclarations,
  checkStatement,
  checkExpression
) where

import Prelude hiding (span)
import Data.List hiding (span)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Map as Map

import Control.Monad.Trans.Writer

import qualified Syntax
import qualified Type.Internal as Internal
import qualified Unicode

import Type.Common


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

    functions = NonEmpty.fromList
      [
        Map.empty,

        Map.fromList [
          (Unicode.collate "integer", [([Integer], Integer), ([Rational], Integer)]),
          (Unicode.collate "rational", [([Integer], Rational), ([Rational], Rational)])
        ]
      ]


checkDeclarations :: Environment -> [Syntax.Declaration ()] -> ([Syntax.Declaration Type], Environment, [Error])
checkDeclarations environment declarations =
  let ((declarations', environment'), errors) = runWriter (Internal.checkDeclarations environment declarations)
   in (declarations', environment', sortOn start errors)


checkStatement :: Type -> Environment -> Syntax.Statement () -> (Syntax.Statement Type, Environment, [Error])
checkStatement expectedType environment statement =
  let ((statement', environment'), errors) = runWriter (Internal.checkStatement expectedType environment statement)
   in (statement', environment', sortOn start errors)


checkExpression :: Environment -> Syntax.Expression () -> (Syntax.Expression Type, Environment, [Error])
checkExpression environment expression =
  let ((expression', environment'), errors) = runWriter (Internal.checkExpression environment expression)
   in (expression', environment', sortOn start errors)
