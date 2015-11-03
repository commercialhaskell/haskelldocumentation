module HigherOrderFunctions.HigherOrderFunctionsSpec where

import qualified HigherOrderFunctions    as HOF

import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "HigherOrderFunctions.singleton" $ do
        it "should contain only the given number" $ do
            map (HOF.singleton 3) [1,2,3,4] `shouldBe` [False, False, True, False]

    describe "HigherOrderFunctions.union" $ do
        let twoOrThree = HOF.singleton 3 `HOF.union` HOF.singleton 2
            expected = 2
            in it "should be the union of two sets" $ do
              map twoOrThree [1,2,3,4] `shouldBe` [False, True, True, False]

    describe "HigherOrderFunctions.difference" $ do
        let evensExceptTwo = HOF.evens `HOF.difference` HOF.singleton 2
         in it "should be the difference of two sets" $ do
              map evensExceptTwo [1,2,3,4] `shouldBe` [False, False, False, True]
