module Span (
  Span (..),
  start,
  end
) where


data Span where
  Span :: Int -> Int -> Span
  deriving (Eq, Show, Read)


start :: Num a => Span -> a
start (Span s _) = fromIntegral s


end :: Num a => Span -> a
end (Span _ e) = fromIntegral e
