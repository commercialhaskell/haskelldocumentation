module Regex where

{-
    This is a bigger exercise where you'll get to use all that you've learned about Haskell.

    The task is to make a simplified regex engine for searching in text. If you've ever used
    the command line tool grep you know how this works.

    To get your foot in the door start by implementing a simple search for substrings.
    Then, add support for special characters like . * ?

    Here's a quick overview over the semantics of the special characters:

        . (dot)  - matches any _one_ character
        *        - matches the preceding pattern element zero or more times
        ?        - matches the preceding pattern element zero or one times

    By running the main method (stack runghc Regex/Regex.hs) you can see how your function responds to different input.
    There are also a few test cases in the Test module you can look at.
-}

-- input: two strings, output: a list of strings representing the hits
find :: String -> String -> [String]
find regex text = []

main = print (find "string" "substringsub")
