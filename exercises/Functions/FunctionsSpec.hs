module Functions.FunctionsSpec where

import qualified Functions

import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "Functions.multiply10by20" $
        it "returns 200" $
            Functions.multiply10by20 `shouldBe` 200

    describe "Functions.plus" $
        it "adds to *arbitrary* numbers" $
            QC.property $ \x y -> Functions.plus x y == x + y

    describe "Functions.sum3" $
        it "adds three *arbitrary* numbers" $
            QC.property $ \x y z -> Functions.sum3 x y z == x + y + z

    describe "Functions.isDollar" $ do
        it "returns true for '$'" $
            Functions.isDollar '$' `shouldBe` True
        it "returns false for anything but '$'" $
            any Functions.isDollar ['%'..'z'] `shouldBe` False

    describe "Functions.xor" $
        it "is correct" $
          QC.property $ \x y -> Functions.xor x y ==  (x && not y) || (y && not x)
