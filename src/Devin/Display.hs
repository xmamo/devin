module Devin.Display (Display (..)) where


class Display a where
  {-# MINIMAL display | displays #-}

  display :: a -> String
  display x = displays x ""


  displays :: a -> ShowS
  displays x = showString (display x)
