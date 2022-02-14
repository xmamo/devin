module Devin.Utils (
  showsRatio,
  showRatio
) where

import Data.Ratio


showsRatio :: (Integral a, Show a) => Ratio a -> ShowS
showsRatio ratio =
  (if ratio < 0 then showChar '-' else id) . shows d . showChar '.' . go m
  where
    num = abs (numerator ratio)
    den = abs (denominator ratio)
    (d, m) = num `divMod` den

    go 0 = showChar '0'
    go m = let (d', m') = (10 * m) `divMod` den in shows d' . (if m' /= 0 then go m' else id)


showRatio :: (Integral a, Show a) => Ratio a -> String
showRatio ratio = showsRatio ratio ""
