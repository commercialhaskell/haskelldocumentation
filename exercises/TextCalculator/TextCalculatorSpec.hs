module TextCalculator.TextCalculatorSpec where

import qualified TextCalculator as TC

import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "TextCalculator.textSum" $
        it "sums numbers with no carrying needed" $
            TC.textSum "123" "456" `shouldBe` "579"

    describe "TextCalculator.textSum" $
        it "sums numbers where carrying is needed" $
            TC.textSum "129" "456" `shouldBe` "585"

    describe "TextCalculator.textSum" $
        it "sums numbers where carrying is needed" $
            TC.textSum "48" "960" `shouldBe` "1008"

    describe "TextCalculator.textSum" $
        it "sums random (positive) numbers" $
            QC.quickCheck $ QC.forAll positivePairs (\(x, y) -> TC.textSum (show x) (show y) == (show (x + y)))

    describe "TextCalculator.textSub" $
        it "subtract simple numbers" $
            TC.textSub "5" "3" `shouldBe` "2"

    describe "TextCalculator.textSub" $
        it "subtract harder numbers" $
            TC.textSub "10" "3" `shouldBe` "7"

    describe "TextCalculator.textMul" $
        it "multiplicates 48 with 10" $
            TC.textMul "48" "10" `shouldBe` "480"

    describe "TextCalculator.textMul" $
        it "multiplicates random (positive) numbers" $
            QC.quickCheck $ QC.forAll positivePairs (\(x, y) -> TC.textMul (show x) (show y) == (show (x * y)))

positivePairs :: QC.Gen (Integer, Integer)
positivePairs =
    do x <- QC.arbitrary
       y <- QC.arbitrary
       return (abs x, abs y)
