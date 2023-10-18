module Devin.Ratio (
  module Data.Ratio,
  displayRatio,
  displaysRatio,
) where

import Data.Ratio


displayRatio :: (Integral a, Show a) => Ratio a -> String
displayRatio ratio = displaysRatio ratio ""


displaysRatio :: (Integral a, Show a) => Ratio a -> ShowS
displaysRatio ratio = case denominator ratio of
  0 -> case compare (numerator ratio) 0 of
    LT -> showString "-∞"
    EQ -> showString "NaN"
    GT -> showString "∞"

  _ ->
    let num = abs (numerator ratio)
        den = abs (denominator ratio)
        (d, m) = num `divMod` den

        go 0 = showChar '0'
        go m | (d', 0) <- (10 * m) `divMod` den = shows d'
        go m | (d', m') <- (10 * m) `divMod` den = shows d' . go m'

    in (if ratio < 0 then showChar '-' else id) . shows d . showChar '.' . go m
