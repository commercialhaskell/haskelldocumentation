module TextCalculator where

import Data.Char

type DigitList    = [Int]
type Calculation  = String -> String -> String

stringNumberToDigitList :: String -> DigitList
stringNumberToDigitList = map digitToInt

digitListToString :: DigitList -> String
digitListToString = map intToDigit

calculationNotYetImplemented :: String -> String -> String -> String
calculationNotYetImplemented x y op = x ++ op  ++ y ++ " is not yet implemented"

{-
    Exercise 1
    Remember how you added numbers when you were a child? Aligning two numbers, then adding two-by-two from right to left,
    sometimes also with a 'carry'?
    Try to do this in Haskell - considering two numbers as Strings (which are lists of Char) like this:

    123 + 456 = 123
                456
              = 579

    If this seems hard, try to make a very simple function first, as the tests for this function vary in requirements.
-}
textSum :: Calculation
textSum x y = calculationNotYetImplemented x y "+" -- TODO Replace with your implementation

{-
    Exercise 2
    Do the same with subtraction.
-}
textSub :: Calculation
textSub x y = calculationNotYetImplemented x y "-" -- TODO Replace with your implementation

{-
    Exercise 3
    Do the same for multiplication.
    Hint/suggestion: Re-use textSum.
-}
textMul :: Calculation
textMul x y = calculationNotYetImplemented x y "*" -- TODO Replace with your implementation


