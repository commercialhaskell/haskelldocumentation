{-# LANGUAGE ScopedTypeVariables #-}

module Maybe.MaybeSpec where

import Prelude hiding (Maybe (..))
import qualified Maybe
import Maybe (Maybe (..))
import Data.List (foldl', nub, sort)
import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "Maybe.safeDiv" $ do
        it "make sure divide by non-zero denominator returns the same result as `div`" $ do
            let propDiv :: Int -> Int -> Bool
                propDiv _ 0 = True
                propDiv a b = case Maybe.safeDiv a b of
                                   Just r -> r == a `div` b
                                   Nothing -> False
             in QC.quickCheck propDiv

    describe "Maybe.safeDiv" $ do
        it "make sure divide by zero returns `Nothing`" $ do
            Maybe.safeDiv 1 0 `shouldBe` Nothing

    describe "Maybe.safeHead" $ do
        it "make sure that safeHead returns the same result as `head` for non-empty lists" $ do
            let propHead :: [Int] -> Bool
                propHead [] = True
                propHead xs = case Maybe.safeHead xs of
                                   Just r -> r == head xs
                                   Nothing -> False
             in QC.quickCheck propHead

    describe "Maybe.safeHead" $ do
        it "make sure `safeHead` returns `Nothing` for an empty list" $ do
            Maybe.safeHead ([] :: [Int]) `shouldBe` Nothing

    describe "Maybe.f" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 0 `shouldBe` Nothing

    describe "Maybe.f" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 4 `shouldBe` Nothing

    describe "Maybe.f" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 2 `shouldBe` Just 1
    
