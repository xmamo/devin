module Parser.Input (
  Input (..),
  position,
  rest
) where

import Data.Text (Text)


data Input = Input Int Text
  deriving (Eq, Show, Read)


position :: Integral a => Input -> a
position (Input p _) = fromIntegral p


rest :: Input -> Text
rest (Input _ r) = r
