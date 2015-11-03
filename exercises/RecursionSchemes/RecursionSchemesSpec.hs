module RecursionSchemes.RecursionSchemesSpec where

import qualified RecursionSchemes        as RS

import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "RecursionSchemes.add1" $ do
        it "adds 1 to all elements in an *arbitrary* list" $ do
            QC.property $ \xs -> RS.add1 xs == map (+1) xs

    describe "RecursionSchemes.numsAsStrings" $ do
        it "converts all integers to strings" $ do
            QC.property $ \xs -> RS.numsAsStrings xs == map show xs

    describe "RecursionSchemes.greaterThan2" $ do
        it "returns integers greater than 2" $ do
            QC.property $ \xs -> RS.greaterThan2 xs == filter (> 2) xs

    describe "RecursionSchemes.filterNot" $ do
        it "keeps the elements that filter would remove for condition (< 2)" $ do
            QC.property $ \xs -> RS.filterNot (< 2) xs == filter (not . (< 2)) xs
