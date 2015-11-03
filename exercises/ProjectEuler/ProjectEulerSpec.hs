module ProjectEuler.ProjectEulerSpec where

import qualified ProjectEuler            as PE

import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "Project Euler - Problem 1" $
        it "is correct" $
            PE.problem1 `shouldBe` (2^4 * 13 * 19 * 59)

    describe "Project Euler - Problem 4" $
        it "is correct" $
            PE.problem4 `shouldBe` (3 * 11 * 83 * 331)

    describe "Project Euler - Problem 6" $
        it "is correct" $
            PE.problem6 `shouldBe` (2 * 3 * 5^2 * 11 * 101 * 151)

    describe "Project Euler - Problem 16" $
        it "is correct" $
            PE.problem16 `shouldBe` (2 * 683)
