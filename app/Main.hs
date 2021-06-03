module Main (main) where

import qualified GI.GLib as GLib

import qualified "devin" Main as Devin


main :: IO ()
main = do
  GLib.setenv "LC_ALL" "C" True
  Devin.main
