module Input (Input (..)) where

import Data.Text (Text)


data Input where
  Input :: {position :: Int, text :: Text} -> Input
  deriving (Eq, Show, Read)
