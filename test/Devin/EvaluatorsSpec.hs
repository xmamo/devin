{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Devin.EvaluatorsSpec (spec) where

import Devin.Display
import Devin.Evaluator
import Devin.Evaluators
import Devin.Parsec
import Devin.Parsers qualified as Parsers

import Test.Hspec


spec :: Spec
spec = do
  describe "evaluateDevin" $ do
    it "should succeed on program 1" $ do
      executionShouldSucceed
        "def main() {\n\
        \    var x = 1;\n\
        \    var y = 2;\n\
        \    var z = 2 * y + x;\n\
        \    assert z == 5;\n\
        \}"

    it "should succeed on program 2" $ do
      executionShouldSucceed
        "def main() {\n\
        \    var array1 = [4, -2, 1, 0];\n\
        \    var array2 = array1;\n\
        \    array1[1] = 7;\n\
        \    assert array1 == [4, 7, 1, 0];\n\
        \    assert array2 == [4, -2, 1, 0];\n\
        \}"

    it "should succeed on program 3" $ do
      executionShouldSucceed
        "def main()\n\
        \    assert sum(34, 35) == 69;\n\
        \\n\
        \def sum(a, b)\n\
        \    return a + b;"

    it "should succeed on program 4" $ do
      executionShouldSucceed
        "def main()\n\
        \    assert factorial(6) == 720;\n\
        \\n\
        \def factorial(n) {\n\
        \    assert n >= 0;\n\
        \\n\
        \    if n == 0\n\
        \        return 1;\n\
        \\n\
        \    return n * factorial(n - 1);\n\
        \}"

    it "should succeed on program 5" $ do
      executionShouldSucceed
        "def main() {\n\
        \    var array = [9, 7, 2, 5];\n\
        \    update(array, 1, -42);\n\
        \    assert array == [9, -42, 2, 5];\n\
        \}\n\
        \\n\
        \def update(ref array, index, value)\n\
        \    array[index] = value;"

    it "should succeed on program 6" $ do
      executionShouldSucceed
        "def main() {\n\
        \    var array = [9, 7, 2, 5];\n\
        \    noupdate(array, 1, -42);\n\
        \    assert array == [9, 7, 2, 5];\n\
        \}\n\
        \\n\
        \def noupdate(array, index, value)\n\
        \    array[index] = value;"

    it "should succeed on program 7" $ do
      executionShouldSucceed
        "def main() {\n\
        \    assert isOdd(69);\n\
        \    assert isEven(420);\n\
        \}\n\
        \\n\
        \def isEven(n) {\n\
        \    assert n >= 0;\n\
        \\n\
        \    if n == 0\n\
        \        return true;\n\
        \    else\n\
        \        return isOdd(n - 1);\n\
        \}\n\
        \\n\
        \def isOdd(n) {\n\
        \    assert n >= 0;\n\
        \\n\
        \    if n == 0\n\
        \        return false;\n\
        \    else\n\
        \        return isEven(n - 1);\n\
        \}"

    it "should succeed on program 8" $ do
      executionShouldSucceed
        "def main() {\n\
        \    var c = -1;\n\
        \\n\
        \    def count() {\n\
        \        c += 1;\n\
        \        return c;\n\
        \    }\n\
        \\n\
        \    assert count() == 0;\n\
        \    assert count() == 1;\n\
        \    assert count() == 2;\n\
        \    assert count() == 3;\n\
        \}"


executionShouldSucceed :: String -> Expectation
executionShouldSucceed source = case runParser Parsers.devin [] "" (0, source) of
  Left parseError -> expectationFailure (show parseError)

  Right devin -> do
    state <- makePredefinedState

    runEvaluator (evaluateDevin devin) state >>= \case
      (Left error, _) -> expectationFailure (display error)
      (Right _, _) -> pure ()
