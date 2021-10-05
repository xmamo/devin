module Type (
  Type (..),
  label,
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
  Function :: {parameters :: [Type], result :: Type} -> Type
  Unknown :: {name :: Text} -> Type
  Error :: Type
  deriving (Eq, Show, Read)


label :: Type -> Text
label Undefined = "?"
label Unit = "Unit"
label Bool = "Bool"
label Int = "Int"
label Float = "Float"
label Function{parameters, result} = "(" <> Text.intercalate ", " (label <$> parameters) <> ") → " <> label result
label Unknown{name} = name
label Error = "⊥"


areCompatible :: Type -> Type -> Bool
areCompatible Error _ = True
areCompatible _ Error = True
areCompatible type1 type2 = type1 == type2
