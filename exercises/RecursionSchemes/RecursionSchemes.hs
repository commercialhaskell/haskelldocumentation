module RecursionSchemes where

{-
    This is what we all came here for. Finally we'll start using higher order functions!
    First a few examples:
-}
celebrities =
    ["conan o'brien", "steve wozniak",
    "Beyoncé", "Queen Latifah",
    "simon peyton jones", "Taylor Swift"]

nameLengths = map length celebrities
-- [13,13,7,13,18,12]

reversedNames = map reverse celebrities
-- ["neirb'o nanoc","kainzow evets","écnoyeB","hafitaL neeuQ","senoj notyep nomis","tfiwS rolyaT"]

isSimon = filter (== "simon peyton jones") celebrities
-- ["simon peyton jones"]

startsWithS = filter (\celeb -> head celeb == 's' ) celebrities
-- ["steve wozniak","simon peyton jones"]
{-
    Whoa, new stuff! There's a lambda function in there:
        (\celeb -> head celeb == 's' )
    Here we bind the first (and only) argument to the name celeb, then
    in the function body we take the head of celeb and checks if the character is an 's'

    Let's look at another lambda function:
-}

startsWithT = filter (\(x:xs) -> x == 'T') celebrities
-- ["Taylor Swift"]
{-
    Would you look at that! We can even pattern match *inside* lambdas.
-}


{-
    Exercise:
    Define the add1 function so it adds 1 to all the numbers in a list
-}
add1 :: [Int] -> [Int]
add1 list = _YOUR_CODE_HERE

{-
    Exercise:
    Define the numsAsStrings function so it converts the integers in a list to strings.

    tip: the `show` function converts ints to strings
-}
numsAsStrings :: [Int] -> [String]
numsAsStrings list = _YOUR_CODE_HERE

{-
    Exercise:
    Define the greaterThan2 function so it returns the integers from a list
    that are greater than 2
-}
greaterThan2 :: [Int] -> [Int]
greaterThan2 list = _YOUR_CODE_HERE

{-
    Exercise:
    Define the filterNot function that works just like filter but inverts the condition.
    In other words, filterNot will *keep* the exact elements that filter would *remove*
-}
filterNot :: (Int -> Bool) -> [Int] -> [Int]
filterNot condition list = _YOUR_CODE_HERE

_YOUR_CODE_HERE = undefined -- ignore me
