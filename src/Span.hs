module Span (Span (..)) where


class Span a where
  start :: Integral b => a -> b

  end :: Integral b => a -> b


instance Integral a => Span (a, a) where
  start = fromIntegral . fst
  end = fromIntegral . snd
