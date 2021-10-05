module Typer.Environment (
  Environment (..),
  predefined
) where

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map

import CallTarget (CallTarget)
import qualified CallTarget
import Type (Type)
import qualified Type


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
        ("Unit", Type.Unit),
        ("Bool", Type.Bool),
        ("Int", Type.Int),
        ("Float", Type.Float)
      ]

    variables = Map.fromList
      [
        ("unit", Type.Unit),
        ("true", Type.Bool),
        ("false", Type.Bool)
      ]

    functions =
      [
        Map.empty,

        Map.fromList [
          ("int", [
            ([Type.Int], Type.Int, CallTarget.IntToInt),
            ([Type.Float], Type.Int, CallTarget.FloatToInt)
          ]),

          ("float", [
            ([Type.Int], Type.Float, CallTarget.IntToFloat),
            ([Type.Float], Type.Float, CallTarget.FloatToFloat)
          ])
        ]
      ]
