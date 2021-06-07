module ResultSpec (spec) where

import Test.QuickCheck.Classes

import Test.Hspec
import Test.Hspec.Checkers

import Result (Result)

import Instances ()


spec :: Spec
spec = describe "Result" do
  testBatch (functor (undefined :: Result (String, String, String)))
  testBatch (foldable (undefined :: Result (String, String, String, Integer, String)))
  testBatch (foldableFunctor (undefined :: Result (String, String)))
  testBatch (traversable (undefined :: Result (String, String, String)))
