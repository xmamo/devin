{-# LANGUAGE ApplicativeDo #-}

module Devin.TypersSpec (spec) where

import Data.List

import Test.Hspec

import Devin.Display
import Devin.Parsec
import Devin.Parsers qualified as Parsers
import Devin.Typer
import Devin.Typers


spec :: Spec
spec = do
  describe "checkDevin" $ do
    it "should succeed on program 1" $ do
      typeCheckingShouldSucceed
        "def main()\n\
        \    var x = sum(1, 2.0);\n\
        \\n\
        \def sum(a: Int, b) -> Int\n\
        \     return a + b;"

    it "should fail on program 2" $ do
      typeCheckingShouldFail
        "def main()\n\
        \    var x = sum(1, 2.0);\n\
        \\n\
        \def sum(a: Int, b: Int) -> Int\n\
        \     return a + b;"

    it "should succeed on program 3" $ do
      typeCheckingShouldSucceed
        "def main()\n\
        \    var x = sum(1, 2);\n\
        \\n\
        \def sum(a: Int, b: Int) -> Int\n\
        \     return a + b;"


typeCheckingShouldSucceed :: String -> Expectation
typeCheckingShouldSucceed source = case runParser Parsers.devin [] "" (0, source) of
  Left parseError -> expectationFailure (show parseError)

  Right devin -> case runTyper (checkDevin devin) predefinedEnvironment of
    (_, _, []) -> pure ()
    (_, _, errors) -> expectationFailure (intercalate "\n" (map display errors))


typeCheckingShouldFail :: String -> Expectation
typeCheckingShouldFail source = case runParser Parsers.devin [] "" (0, source) of
  Left parseError -> expectationFailure (show parseError)

  Right devin -> case runTyper (checkDevin devin) predefinedEnvironment of
    (_, _, []) -> expectationFailure "Expected type checking to fail, but it succeeded"
    (_, _, _) -> pure ()
