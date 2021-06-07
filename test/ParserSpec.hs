module ParserSpec (spec) where

import Test.QuickCheck.Classes

import Test.Hspec
import Test.Hspec.Checkers

import Parser (Parser, ParserT)

import Instances ()


instance Show (ParserT a b) where
  show _ = "Parser"


spec :: SpecWith ()
spec = describe "Parser" do
  testBatch (functor (undefined :: Parser (String, String, String)))
  testBatch (applicative (undefined :: Parser (String, String, String)))
  testBatch (monad (undefined :: Parser (String, String, String)))
  testBatch (monadFunctor (undefined :: Parser (String, String)))
  testBatch (monadApplicative (undefined :: Parser (String, String)))
  testBatch (alternative (undefined :: Parser String))
