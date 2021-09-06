module Typer.Environment (
  Environment (..),
  predefined
) where

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map

import CallTarget (CallTarget (BuiltinInt))
import qualified CallTarget
import Type (Type)
import qualified Type
import qualified Unicode


data Environment = Environment {
  types :: Map Text Type,
  variables :: Map Text Type,
  functions :: [Map Text [([Type], Type, CallTarget)]]
} deriving (Eq, Show, Read)


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
          (Unicode.collate "int", [
            ([Type.Int], Type.Int, CallTarget.BuiltinInt),
            ([Type.Float], Type.Int, CallTarget.BuiltinInt)
          ]),

          (Unicode.collate "float", [
            ([Type.Int], Type.Float, CallTarget.BuiltinFloat),
            ([Type.Float], Type.Float, CallTarget.BuiltinFloat)
          ])
        ]
      ]
