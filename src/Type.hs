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
    types =
      [
        (Unicode.collate "Unit", Unit),
        (Unicode.collate "Bool", Bool),
        (Unicode.collate "Int", Int),
        (Unicode.collate "Float", Float)
      ]

    variables =
      [
        (Unicode.collate "unit", Unit),
        (Unicode.collate "true", Bool),
        (Unicode.collate "false", Bool)
      ]

    functions =
      [
        [],

        [
          (Unicode.collate "int", [Int], Int),
          (Unicode.collate "int", [Float], Int),
          (Unicode.collate "float", [Int], Float),
          (Unicode.collate "float", [Float], Float)
        ]
      ]
