module Type (
  Type (..),
  label,
  areCompatible
) where

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Unicode


data Type where
  Unit :: Type
  Boolean :: Type
  Integer :: Type
  Rational :: Type
  Function :: {parameters :: [Type], result :: Type} -> Type
  Unknown :: {name :: Text} -> Type
  Error :: Type
  deriving (Show, Read)


instance Eq Type where
  Unit == Unit = True
  Boolean == Boolean = True
  Integer == Integer = True
  Rational == Rational = True
  Function parameters1 result1 == Function parameters2 result2 = parameters1 == parameters2 && result1 == result2
  Unknown name1 == Unknown name2 = Unicode.collate name1 == Unicode.collate name2
  Error == Error = True
  _ == _ = False


label :: Type -> Text
label Unit = "Unit"
label Boolean = "Boolean"
label Integer = "Integer"
label Rational = "Rational"
label Function{parameters, result} = "(" <> Text.intercalate ", " (label <$> parameters) <> ") → " <> label result
label Unknown{name} = name
label Error = "⊥"


areCompatible :: Type -> Type -> Bool
areCompatible Error  _ = True
areCompatible _ Error = True
areCompatible type1 type2 = type1 == type2
