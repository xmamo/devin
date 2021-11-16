module Main (main) where

import System.Environment

import qualified "devin" Main as Devin


main :: IO ()
main = do
  setEnv "LC_ALL" "C"
  Devin.main
