module Recursion.RecursionSpec where

import qualified Recursion               as R

import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "Recursion.secondToLast" $ do
        let input = [1,2,3,4]
            expected = 3
        it ("returns the second to last element in " ++ show input) $ do
            R.secondToLast input `shouldBe` expected

    describe "Recursion.listLength" $ do
        it "returns the length of an *arbitrary* list" $ do
            QC.property $ \x -> R.listLength x == length x
