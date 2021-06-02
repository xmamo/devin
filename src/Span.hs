module Span (
  Span (..),
  start,
  end
) where


data Span where
  Span :: Int -> Int -> Span
  deriving (Eq, Show, Read)


start :: Integral a => Span -> a
start (Span s _) = fromIntegral s


end :: Integral a => Span -> a
end (Span _ e) = fromIntegral e
