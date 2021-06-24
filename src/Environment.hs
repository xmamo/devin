module Environment (
  Environment (..),
  predefined
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map

import Type (Type)
import qualified Type
import qualified Unicode


data Environment where
  Environment :: {
    types :: Map Text Type,
    variables :: Map Text Type,
    functions :: NonEmpty (Map Text [([Type], Type)])
  } -> Environment

  deriving (Eq, Show, Read)


predefined :: Environment
predefined = Environment types variables functions
  where
    types = Map.fromList
      [
        (Unicode.collate "Unit", Type.Unit),
        (Unicode.collate "Boolean", Type.Boolean),
        (Unicode.collate "Integer", Type.Integer),
        (Unicode.collate "Rational", Type.Rational)
      ]

    variables = Map.fromList
      [
        (Unicode.collate "unit", Type.Unit),
        (Unicode.collate "true", Type.Boolean),
        (Unicode.collate "false", Type.Boolean)
      ]

    functions = NonEmpty.fromList
      [
        Map.empty,

        Map.fromList [
          (Unicode.collate "integer", [([Type.Integer], Type.Integer), ([Type.Rational], Type.Integer)]),
          (Unicode.collate "rational", [([Type.Integer], Type.Rational), ([Type.Rational], Type.Rational)])
        ]
      ]
