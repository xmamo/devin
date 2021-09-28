module Parser.Input (Input (..)) where

import Data.Text (Text)


data Input = Input {position :: Int, rest :: Text}
  deriving (Eq, Show, Read)
