module Devin.Utils (
  showsRatio,
  showRatio
) where

import Data.Ratio


showsRatio :: (Integral a, Show a) => Ratio a -> ShowS
showsRatio ratio =
  let num = abs (numerator ratio)
      den = abs (denominator ratio)
      (d, m) = num `divMod` den

      go 0 = showChar '0'
      go m | (d', 0) <- (10 * m) `divMod` den = shows d'
      go m | (d', m') <- (10 * m) `divMod` den = shows d' . go m'

   in (if ratio < 0 then showChar '-' else id) . shows d . showChar '.' . go m


showRatio :: (Integral a, Show a) => Ratio a -> String
showRatio ratio = showsRatio ratio ""
