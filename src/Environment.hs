module Environment (
  Environment (..),
  predefined
) where

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
    functions :: [Map Text [([Type], Type)]]
  } -> Environment

  deriving (Eq, Show, Read)


predefined :: Environment
predefined = Environment types variables functions
  where
    types = Map.fromList
      [
        (Unicode.collate "Unit", Type.Unit),
        (Unicode.collate "Bool", Type.Bool),
        (Unicode.collate "Int", Type.Int),
        (Unicode.collate "Float", Type.Float)
      ]

    variables = Map.fromList
      [
        (Unicode.collate "unit", Type.Unit),
        (Unicode.collate "true", Type.Bool),
        (Unicode.collate "false", Type.Bool)
      ]

    functions =
      [
        Map.empty,

        Map.fromList [
          (Unicode.collate "int", [([Type.Int], Type.Int), ([Type.Float], Type.Int)]),
          (Unicode.collate "float", [([Type.Int], Type.Float), ([Type.Float], Type.Float)])
        ]
      ]
