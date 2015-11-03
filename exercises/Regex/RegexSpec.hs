module Regex.RegexSpec where

import qualified Regex                  as Regex

import qualified Test.QuickCheck as QC
import Test.Hspec

spec =
  describe "Regex.find" $ do
        regexTest "sub" "substringsubstring" ["sub", "sub"]

        let needle = "2312"
            haystack = "badabiiiing231"
         in it ("does not find '" ++ needle ++ "' in '" ++ haystack ++ "'") $ do
              Regex.find needle haystack `shouldBe` []

        regexTest "su." "oosubstr" ["sub"]
        regexTest ".i.of" "aa1i1ofs" ["1i1of"]

        regexTest "su*" "papasuubstring" ["suu", "s"]
        regexTest "ab*" "abbbba" ["abbbb", "a"]
        regexTest "a*b" "accbaaabsdab" ["b", "aaab", "ab"]

        regexTest "92?3?" "125s29242s" ["92"]

regexTest needle haystack expected =
    it ("'" ++ needle ++ "' in '" ++ haystack ++ "'") $ do
        Regex.find needle haystack `shouldBe` expected
