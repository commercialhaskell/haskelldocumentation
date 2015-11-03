module Vigenere.VigenereSpec where

import qualified Test.QuickCheck as QC
import qualified Vigenere as VI
import Test.Hspec

spec = do

    -- Encrypting single characters
    describe "Vigenere.encryptChar" $ do
        it "encrypts 'A' and 'L' as 'L'" $ do
            VI.encryptChar ('A', 'L') `shouldBe` 'L'
    describe "Vigenere.encryptChar" $ do
        it "encrypts 'C' and 'N' as 'P'" $ do
            VI.encryptChar ('C', 'N') `shouldBe` 'P'

    -- Decrypting single characters
    describe "Vigenere.decryptChar" $ do
        it "decrypts 'F' and 'M' as 'T'" $ do
            VI.decryptChar ('F', 'M') `shouldBe` 'T'
    describe "Vigenere.decryptChar" $ do
        it "decrypts 'H' and 'L' as 'W'" $ do
            VI.decryptChar ('H', 'L') `shouldBe` 'W'

    -- Encrypting strings
    describe "Vigenere.encrypt" $ do
        it "encrypts \"ATTACKATDAWN\" \"LEMON\" as \"LXFOPVEFRNHR\"" $ do
            VI.encrypt "ATTACKATDAWN" "LEMON" `shouldBe` "LXFOPVEFRNHR"

    -- Decrypting strings
    describe "Vigenere.decrypt" $ do
        it "decrypts \"LXFOPVEFRNHR\" \"LEMON\" as \"ATTACKATDAWN\"" $ do
            VI.decrypt "LXFOPVEFRNHR" "LEMON" `shouldBe` "ATTACKATDAWN"

    -- Encrypt, then decrypt strings
    describe "Vigenere.decrypt.encrypt" $ do
        it "encrypts then decrypts properly" $ do
            (VI.decrypt (VI.encrypt "SECRET" "MYTEXT") "SECRET") == "MYTEXT"
