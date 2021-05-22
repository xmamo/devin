module Input (
  Input (..),
  position,
  text
) where

import Data.Text (Text)


data Input where
  Input :: Int -> Text -> Input
  deriving (Eq, Show, Read)


position :: Num a => Input -> a
position (Input p _) = fromIntegral p


text :: Input -> Text
text (Input _ t) = t
