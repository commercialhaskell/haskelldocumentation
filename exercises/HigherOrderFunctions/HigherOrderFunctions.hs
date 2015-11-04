module HigherOrderFunctions where

{-
    Now for some potentially mind blowing business: we're going to implement
    sets of integers, as functions!
    The idea is that a set is a boolean function for all integers, and
    returns true if a given integer is part of the set and false otherwise.
-}

-- We create a type alias to avoid repetition of (Int -> Bool)
type Set = (Int -> Bool)

{-
    Sets are then defined as functions that returns true/false based on the
    number they're given.
-}

-- The set of even numbers
evens :: Set
evens n = n `mod` 2 == 0

-- The set of positive numbers
positives :: Set
positives n = n > 0

-- The set of numbers divisible by 3
divBy3 :: Set
divBy3 n = n `mod` 3 == 0

-- The empty set
emptySet :: Set
emptySet _ = False

{-
    The lightness of sets as functions makes it very cheap to
    combine them with set operators, such as intersect and union.
-}

-- The intersection of two sets is a function that returns a function that
-- takes `x` and checks if it's a member of both sets.
intersect :: Set -> Set -> Set
intersect a b = \x -> let memberOfA = a x
                          memberOfB = b x
                       in memberOfA && memberOfB

-- Combinations
mySet1 = intersect evens divBy3

{-
    Exercise:
    Write a function that returns a set containing only the provided argument.
-}

singleton :: Int -> Set
singleton n = _YOUR_CODE_HERE

{-
    Exercise:
    Write a function for set union.
-}

union :: Set -> Set -> Set
union a b = _YOUR_CODE_HERE

{-
    Exercise:
    Write a function for set difference. Given two sets a and b, the result
    should be a set with only those numbers that appear in set a, but not is
    set b.
-}

difference :: Set -> Set -> Set
difference a b = _YOUR_CODE_HERE

_YOUR_CODE_HERE = undefined -- ignore me
