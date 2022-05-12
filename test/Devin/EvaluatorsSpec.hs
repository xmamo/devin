{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Devin.EvaluatorsSpec (spec) where

import Devin.Display
import Devin.Evaluator
import Devin.Evaluators
import Devin.Parsec
import qualified Devin.Parsers as Parsers

import Test.Hspec


spec :: Spec
spec = do
  describe "evalDevin" $ do
    it "should succeed on program 1" $
      executionShouldSucceed
        "def main() {\n\
        \    var x = 1;\n\
        \    var y = 2;\n\
        \    var z = 2 * y + x;\n\
        \    assert z == 5;\n\
        \}"

    it "should succeed on program 2" $
      executionShouldSucceed
        "def main() {\n\
        \    var array1 = [4, -2, 1, 0];\n\
        \    var array2 = array1;\n\
        \    array1[1] = 7;\n\
        \    assert array1 == [4, 7, 1, 0];\n\
        \    assert array2 == [4, -2, 1, 0];\n\
        \}"

    it "should succeed on program 3" $
      executionShouldSucceed
        "def main() {\n\
        \    var array = [1, 2];\n\
        \    assert array * 5 == [1, 2, 1, 2, 1, 2, 1, 2, 1, 2];\n\
        \    assert array * 0 == [];\n\
        \    assert array * -2 == [];\n\
        \}"

    it "should succeed on program 4" $
      executionShouldSucceed
        "def main()\n\
        \    assert sum(34, 35) == 69;\n\
        \\n\
        \def sum(a, b)\n\
        \    return a + b;"

    it "should succeed on program 5" $
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

    it "should succeed on program 6" $
      executionShouldSucceed
        "def main()\n\
        \    assert factorial(6) == 720;\n\
        \\n\
        \def factorial(n) {\n\
        \    assert n >= 0;\n\
        \    var result = 1;\n\
        \\n\
        \    while n > 1 {\n\
        \        result *= n;\n\
        \        n -= 1;\n\
        \    }\n\
        \\n\
        \    return result;\n\
        \}"

    it "should succeed on program 7" $
      executionShouldSucceed
        "def main()\n\
        \    assert factorial(6) == 720;\n\
        \\n\
        \def factorial(n) {\n\
        \    if n == 0\n\
        \        return 1;\n\
        \\n\
        \    assert n > 0;\n\
        \    var result = 1;\n\
        \\n\
        \    do {\n\
        \        result *= n;\n\
        \        n -= 1;\n\
        \    } while n > 1;\n\
        \\n\
        \    return result;\n\
        \}"

    it "should succeed on program 8" $
      executionShouldSucceed
        "def main() {\n\
        \    var array = [9, 7, 2, 5];\n\
        \    update(array, 1, -42);\n\
        \    assert array == [9, -42, 2, 5];\n\
        \}\n\
        \\n\
        \def update(ref array, index, value)\n\
        \    array[index] = value;"

    it "should succeed on program 9" $
      executionShouldSucceed
        "def main() {\n\
        \    var array = [9, 7, 2, 5];\n\
        \    noupdate(array, 1, -42);\n\
        \    assert array == [9, 7, 2, 5];\n\
        \}\n\
        \\n\
        \def noupdate(array, index, value)\n\
        \    array[index] = value;"

    it "should succeed on program 10" $
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

    it "should succeed on program 11" $
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

    it "should succeed on program 12" $
      executionShouldSucceed
        "def main() -> Unit {\n\
        \    var array = [9, 2, 1, 21, -2, 4];\n\
        \    bubbleSort(array);\n\
        \    assert array == [-2, 1, 2, 4, 9, 21];\n\
        \}\n\
        \\n\
        \def bubbleSort(ref array: [Int]) -> Unit {\n\
        \    var i = 0;\n\
        \\n\
        \    while i < len array {\n\
        \        var j = i + 1;\n\
        \\n\
        \        while j < len array {\n\
        \            if array[i] > array[j] {\n\
        \                var t = array[i];\n\
        \                array[i] = array[j];\n\
        \                array[j] = t;\n\
        \            }\n\
        \\n\
        \            j += 1;\n\
        \        }\n\
        \\n\
        \        i += 1;\n\
        \    }\n\
        \}"


executionShouldSucceed :: String -> Expectation
executionShouldSucceed source = case runParser Parsers.devin [] "" (0, source) of
  Left parseError -> expectationFailure (show parseError)

  Right devin -> do
    state <- makePredefinedState

    runEvaluator (evalDevin devin) state >>= \case
      (Left error, _) -> expectationFailure (display error)
      (Right _, _) -> pure ()
