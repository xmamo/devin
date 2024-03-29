{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NegativeLiterals #-}

module Devin.ParsersSpec (spec) where

import Test.Hspec

import Devin.Parsers
import Devin.Syntax


spec :: Spec
spec = do
  describe "expression" $ do
    it "should accept fragment 1" $
      shouldParse expression "1 += 2 " BinaryExpression{
        left = IntegerExpression 1 (0, 1),
        binary = AddAssignOperator (2, 4),
        right = IntegerExpression 2 (5, 6)
      }

    it "should accept fragment 2" $
      shouldParse expression "1+ 2*3 -(1) " BinaryExpression{
        left = BinaryExpression{
          left = IntegerExpression 1 (0, 1),

          binary = AddOperator (1, 2),

          right = BinaryExpression{
            left = IntegerExpression 2 (3, 4),
            binary = MultiplyOperator (4, 5),
            right = IntegerExpression 3 (5, 6)
          }
        },

        binary = SubtractOperator (7, 8),

        right = ParenthesizedExpression{
          open = Token (8, 9),
          inner = IntegerExpression 1 (9, 10),
          close = Token (10, 11)
        }
      }

    it "should accept fragment 3" $
      shouldParse expression "a+ -2.1= f (x, y ,z ) *=88 " BinaryExpression{
        left = VarExpression "a" (0, 1),

        binary = AddOperator (1, 2),

        right = BinaryExpression{
          left = RationalExpression -2.1 (3, 7),

          binary = PlainAssignOperator (7, 8),

          right = BinaryExpression{
            left = CallExpression{
              funId = SymbolId "f" (9, 10),

              open = Token (11, 12),

              args = [
                VarExpression "x" (12, 13),
                VarExpression "y" (15, 16),
                VarExpression "z" (18, 19)
              ],

              commas = [
                Token (13, 14),
                Token (17, 18)
              ],

              close = Token (20, 21)
            },

            binary = MultiplyAssignOperator (22, 24),

            right = IntegerExpression 88 (24, 26)
          }
        }
      }

  describe "binaryOperator" $
    it "should accept fragment 4" $
      shouldParse binaryOperator "+ " (AddOperator (0, 1))


shouldParse :: (Eq a, Show a) => Parser String a -> String -> a -> Expectation
shouldParse parser source x = case parse parser "" (0, source) of
  Left parseError -> expectationFailure (show parseError)
  Right (x', _) -> x' `shouldBe` x
