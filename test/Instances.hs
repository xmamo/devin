module Instances () where

import qualified Data.Text as Text

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Instances.Text ()

import Input (Input (Input))
import Result (Result)
import qualified Result
import Parser (Parser, ParserT (ParserT))
import qualified Parser


instance EqProp Input where
  (=-=) = eq


instance EqProp a => EqProp (Result a) where
  (Result.Success value1 rest1) =-= (Result.Success value2 rest2) = value1 =-= value2 .&&. rest1 =-= rest2

  (Result.Failure isFatal1 position1 expectations1) =-= (Result.Failure isFatal2 position2 expectations2) = conjoin
    [
      isFatal1 =-= isFatal2,
      position1 =-= position2,
      conjoin ((`elem` expectations2) <$> expectations1),
      conjoin ((`elem` expectations1) <$> expectations2)
    ]

  _ =-= _ = property False


instance EqProp a => EqProp (Parser a) where
  parser1 =-= parser2 = forAll arbitrary (\input -> Parser.parse parser1 input =-= Parser.parse parser2 input)


instance Arbitrary Input where
  arbitrary = do
    text <- arbitrary
    position <- arbitrary `suchThat` ((&&) <$> (>= 0) <*> (<= Text.length text))
    pure (Input position text)


instance Arbitrary a => Arbitrary (Result a) where
  arbitrary = oneof
    [
      Result.Success <$> arbitrary <*> arbitrary,
      Result.Failure <$> arbitrary <*> (arbitrary `suchThat` (>= 0)) <*> arbitrary
    ]


instance Arbitrary a => Arbitrary (Parser a) where
  arbitrary = MkGen \r n -> ParserT \input -> pure (unGen (arbitraryResult input) r n)
    where
      arbitraryResult (Input position text) = oneof
        [
          do
            offset <- arbitrary `suchThat` ((&&) <$> (>= 0) <*> (<= Text.length text))
            value <- arbitrary
            pure (Result.Success value (Input (position + offset) (Text.drop offset text))),

          Result.Failure <$> arbitrary <*> pure position <*> arbitrary
        ]
