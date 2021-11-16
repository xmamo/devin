module Range (Range (..)) where


class Range a where
  start :: Num b => a -> b

  end :: Num b => a -> b


instance Integral a => Range (a, a) where
  start (x, _) = fromIntegral x

  end (_, y) = fromIntegral y
