module QuickCheck.QuickCheckSpec where

import qualified QuickCheck      as QCE

import qualified Test.QuickCheck as QC
import Test.Hspec

spec = do
    describe "QuickCheckExamples.encodeDecode" $ do
        it "decode is inverse encode" $ do
            QC.property $ \xs -> xs == (QCE.decode . QCE.encode) xs
