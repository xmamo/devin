module Type (
  Type (..),
  pretty,
  areCompatible
) where

import Data.Text (Text)
import qualified Data.Text as Text


data Type where
  Undefined :: Type
  Unit :: Type
  Bool :: Type
  Int :: Type
  Float :: Type
  Function :: {parameterTypes :: [Type], returnType :: Type} -> Type
  Unknown :: {name :: Text} -> Type
  Error :: Type
  deriving (Eq, Show, Read)


pretty :: Type -> Text
pretty Undefined = "?"
pretty Unit = "Unit"
pretty Bool = "Bool"
pretty Int = "Int"
pretty Float = "Float"
pretty Function{parameterTypes, returnType} = "(" <> Text.intercalate ", " (pretty <$> parameterTypes) <> ") → " <> pretty returnType
pretty Unknown{name} = name
pretty Error = "⊥"


areCompatible :: Type -> Type -> Bool
areCompatible Error _ = True
areCompatible _ Error = True
areCompatible type1 type2 = type1 == type2
