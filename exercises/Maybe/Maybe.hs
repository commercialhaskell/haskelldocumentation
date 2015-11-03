module Maybe where

import Prelude hiding (Maybe (..))

{-
    Sometimes functions need to be able to indicate that they failed to produce
    a result: They could be asked to divide by zero, take the first element of
    an empty list, attempt to fetch a result from a database that went down,
    etc.

    In these cases, the data type `Maybe` comes to your rescue. `Maybe` (often
    called `Option` in other languages) has two constructors:
    `Just` contains a value of any type and indicates a success result, while
    `Nothing` is a nullary constructor that indicates failure.
-}

data Maybe a = Just a
             | Nothing
             deriving (Eq, Ord, Show)

{-
    Finish the implementation of `safeDiv` below to return Nothing if the
    denominator is zero, or Just (a `div` b) if b is non-zero.
-}

safeDiv :: Int -> Int -> Maybe Int
safeDiv a b = _YOUR_CODE_HERE

{-
    Finish the implementation of `safeHead` below to return Nothing if the
    list is empty, or Just <first element> if the list has at least one element.
-}

safeHead :: [a] -> Maybe a
safeHead [] = _YOUR_CODE_HERE
safeHead _Match_Non_empty_List_Here = _YOUR_CODE_HERE

{-
    The great thing with `Maybe` is that you are forced to pattern-match on it
    to extract the result. The compiler will warn you when you don't match all
    cases (in this case: both the `Just` case and the `Nothing` case), which is
    great for making sure that one has covered all error cases.

    To see this in practice, finish the following function that calculates

      (a `div` b) `div` (a-b)

      i.e.         a/b
                  —————
                   a-b

    Of course, instead of using the unsafe `div` function, use the `safeDiv`
    funciton you defined above. :)
-}

f :: Int -> Int -> Maybe Int
f a b = _YOUR_CODE_HERE

{-
    Concluding remarks:

    What if you had even more sub calculations that could fail – wouldn't we
    end up with a horrible “staircase” of `case`s, like this?

    case calcA of
         Nothing -> Nothing
         Just a -> case calcB a of
                        Nothing -> Nothing
                        Just b -> case calcC c of
                                       Nothing -> Nothing
                                       Just c -> ...

    Well, yes. But fear not! You will later learn about how `Maybe` is a
    “monad” in you Haskell adventures. This will allow you to use much nicer
    syntax and you will not be required to do this boring “threading” of
    results.

    “Learn you a Haskell” has a chapter which introduces monads by showing the
    Maybe monad:

      http://learnyouahaskell.com/a-fistful-of-monads
-}

_YOUR_CODE_HERE = undefined -- ignore me
