{-# LANGUAGE ApplicativeDo #-}

module Devin.TypersSpec (spec) where

import Data.List

import Devin.Display
import Devin.Parsec
import Devin.Parsers qualified as Parsers
import Devin.Typer
import Devin.Typers

import Test.Hspec


spec :: Spec
spec = do
  describe "checkDevin" $ do
    it "should succeed on program 1" $ do
      typeCheckingShouldSucceed
        "def main()\n\
        \    var x = sum(1, 2.0);\n\
        \\n\
        \def sum(a: Int, b) -> Int\n\
        \     return 0;"

    it "should fail on program 2" $ do
      typeCheckingShouldFail
        "def main()\n\
        \    var x = sum(1, 2.0);\n\
        \\n\
        \def sum(a: Int, b: Int) -> Int\n\
        \     return 0;"

    it "should succeed on program 3" $ do
      typeCheckingShouldSucceed
        "def main()\n\
        \    var x = sum(1, 2);\n\
        \\n\
        \def sum(a: Int, b: Int) -> Int\n\
        \     return 0;"

    it "should fail on program 4" $ do
      typeCheckingShouldFail
        "def main()\n\
        \    var x = sum([1, false, 3]);\n\
        \\n\
        \def sum(list: [Int]) -> Int\n\
        \    return 0;\n\
        \"

    it "should succeed on program 5" $ do
      typeCheckingShouldSucceed
        "def main() {\n\
        \    def f(x) return x;\n\
        \    var x = sum([f(1), false, 3]);\n\
        \}\n\
        \\n\
        \def sum(list: [Int]) -> Int\n\
        \    return 0;\n\
        \"


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
