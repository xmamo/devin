{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Devin.Interval (Interval (..)) where


class Interval a where
  start :: Num b => a -> b
  end :: Num b => a -> b


instance Integral a => Interval (a, a) where
  start :: Num b => (a, a) -> b
  start interval = fromIntegral (fst interval)


  end :: Num b => (a, a) -> b
  end interval = fromIntegral (snd interval)
