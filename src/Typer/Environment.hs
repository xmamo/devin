module Typer.Environment (
  Environment (..),
  predefined
) where

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map

import CallTarget (CallTarget)
import qualified CallTarget
import Type


data Environment = Environment {
  depth :: Int,
  types :: Map Text Type,
  variables :: Map Text Type,
  functions :: [Map Text [([Type], Type, CallTarget)]]
} deriving (Eq, Show, Read)


predefined :: Environment
predefined = Environment 0 types variables functions
  where
    types = Map.fromList
      [
        ("Unit", Unit),
        ("Bool", Bool),
        ("Int", Int),
        ("Float", Float)
      ]

    variables = Map.fromList
      [
        ("unit", Unit),
        ("true", Bool),
        ("false", Bool)
      ]

    functions =
      [
        Map.empty,

        Map.fromList [
          ("int", [
            ([Int], Int, CallTarget.IntToInt),
            ([Float], Int, CallTarget.FloatToInt)
          ]),

          ("float", [
            ([Int], Float, CallTarget.IntToFloat),
            ([Float], Float, CallTarget.FloatToFloat)
          ])
        ]
      ]
