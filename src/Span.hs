module Span (Span (..)) where


data Span where
  Span :: {start :: Int, end :: Int} -> Span
  deriving (Eq, Show, Read)
