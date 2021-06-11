module Instances () where

import Data.Foldable
import Data.Functor
import Data.Maybe
import Control.Applicative

import qualified Data.Text as Text

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Instances.Text ()

import Input (Input (Input))
import Result (Result)
import qualified Result
import Parser (Parser)
import qualified Parser


instance EqProp Input where
  (=-=) = eq


instance EqProp a => EqProp (Result a) where
  Result.Success value1 rest1 =-= Result.Success value2 rest2 = value1 =-= value2 .&&. rest1 =-= rest2

  Result.Failure isFatal1 position1 expectations1 =-= Result.Failure isFatal2 position2 expectations2 = conjoin
    [
      isFatal1 =-= isFatal2,
      position1 =-= position2,
      conjoin (expectations1 <&> (`elem` expectations2)),
      conjoin (expectations2 <&> (`elem` expectations1))
    ]

  _ =-= _ = property False


instance EqProp a => EqProp (Parser a) where
  parser1 =-= parser2 = forAll arbitrary (\input -> Parser.parse parser1 input =-= Parser.parse parser2 input)


instance Arbitrary Input where
  arbitrary = do
    text <- arbitrary
    position <- chooseInt (0, Text.length text)
    pure (Input position text)


instance Arbitrary a => Arbitrary (Result a) where
  arbitrary = oneof
    [
      Result.Success <$> arbitrary <*> arbitrary,
      Result.Failure <$> arbitrary <*> arbitrarySatisfying (>= 0) <*> arbitrary
    ]


instance (Monoid a, Arbitrary a, CoArbitrary a) => Arbitrary (Parser a) where
  arbitrary = sized (arbitraryParser True)


arbitraryParser :: forall a. (Monoid a, Arbitrary a, CoArbitrary a) => Bool -> Int -> Gen (Parser a)
arbitraryParser allowEmpty = \case
  0 -> oneof $ catMaybes
    [
      -- Parser.position
      toMaybe allowEmpty do
        f <- arbitrary :: Gen (Integer -> a)
        let parser = Parser.position
        pure (f <$> parser),

      -- Parser.char
      Just do
        f <- arbitrary
        parser <- Parser.char <$> arbitrary
        pure (f <$> parser),

      -- Parser.satisfy
      Just do
        f <- arbitrary
        parser <- Parser.satisfy <$> arbitrary
        pure (f <$> parser),

      -- Parser.text
      Just do
        f <- arbitrary
        parser <- Parser.text <$> arbitrarySatisfying (\t -> allowEmpty || not (Text.null t))
        pure (f <$> parser),

      -- Parser.eoi
      toMaybe allowEmpty do
        f <- arbitrary
        pure (f <$> Parser.eoi),

      -- pure
      toMaybe allowEmpty do
        value <- arbitrary
        pure (pure value),

      -- empty
      toMaybe allowEmpty do
        f <- arbitrary :: Gen (() -> a)
        let parser = empty
        pure (f <$> parser)
    ]

  n -> oneof $ catMaybes
    [
      -- Parser.either
      Just do
        let n' = max 0 (n - 2)
        parser1 <- arbitraryParser allowEmpty n'
        parser2 <- arbitraryParser allowEmpty n'
        pure (either id id <$> Parser.either parser1 parser2),

      -- Parser.separatedBy
      toMaybe allowEmpty do
        let n' = max 0 (n - 2)
        parser <- arbitraryParser False n'
        separator <- arbitraryParser True n' :: Gen (Parser ())
        pure (mconcat <$> Parser.separatedBy parser separator),

      -- Parser.separatedBy1
      Just do
        let n' = max 0 (n - 2)
        parser <- arbitraryParser False n'
        separator <- arbitraryParser True n' :: Gen (Parser ())
        pure (mconcat <$> Parser.separatedBy1 parser separator),

      -- Parser.label
      Just do
        let n' = max 0 (n - 1)
        parser <- arbitraryParser allowEmpty n'
        label <- arbitrary
        pure (Parser.label label parser),

      -- Parser.commit
      Just do
        let n' = max 0 (n - 1)
        parser <- arbitraryParser allowEmpty n'
        pure (Parser.commit parser),

      -- (<$>)
      Just do
        let n' = max 0 (n - 1)
        f <- arbitrary :: Gen (a -> a)
        parser <- arbitraryParser allowEmpty n'
        pure (f <$> parser),

      -- (<*>)
      Just do
        let n' = max 0 (n - 2)
        f <- arbitraryParser True n' :: Gen (Parser ((a -> a) -> a))
        parser <- arbitraryParser allowEmpty n'
        pure (f <*> parser),

      -- (<|>)
      Just do
        let n' = max 0 (n - 2)
        parser1 <- arbitraryParser allowEmpty n'
        parser2 <- arbitraryParser allowEmpty n'
        pure (parser1 <|> parser2),

      -- some
      Just do
        let n' = max 0 (n - 1)
        parser <- arbitraryParser False n'
        pure (mconcat <$> some parser),

      -- many
      toMaybe allowEmpty do
        let n' = max 0 (n - 1)
        parser <- arbitraryParser False n'
        pure (mconcat <$> many parser),

      -- optional
      toMaybe allowEmpty do
        let n' = max 0 (n - 1)
        parser <- arbitraryParser False n'
        pure (fold <$> optional parser)
    ]

  where
    toMaybe True x = Just x
    toMaybe False _ = Nothing
