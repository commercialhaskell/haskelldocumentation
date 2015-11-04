module Functions where

{-
    In this module we will learn how to define and apply functions.
-}


-- Defining a function taking one parameter:
welcome name = "Welcome, " ++ name ++ "."
--  ^     ^    `------------^------------'
-- name  parameter         body

{-
    There's no keyword like "def", "function" or anything preceding the name, just:
    <functionName> <arguments> = <body>
-}

-- Calling a function with arguments
welcomeSirOrMadam = welcome "Sir or Madam"

{-
    Now if we do multiple function applications and intend on passing the result
    of the first application to the second application and so on, we would have to
    use parentheses to remove abiguities:
-}
printWelcomeMessage2 = putStrLn (welcome "Welcome to the present")
{-
    otherwise the compiler would interpret that function this way:

printWelcomeMessage2 = (putStrLn welcome) "Welcome to the present"
                           ^        ^                 ^
             function to apply     arg    arg to what (putstrln welcome) returned

    So a general tip is: if in doubt, add parentheses!
-}


{-
    Exercise:
    Use the multiply function to return the product of 10 and 20.
    Fill in your answer as the body of multiply10by20.
-}
-- This function returns the product of its arguments
multiply arg1 arg2 = arg1 * arg2

multiply10by20 = _YOUR_CODE_HERE


{-
    Exercise:
    Define a function, plus, that takes two arguments and returns their sum
-}
plus :: Integer -> Integer -> Integer
plus arg1 arg2 = _YOUR_CODE_HERE


{-
    Exercise:
    Define a function, sum3, that takes 3 arguments and returns their sum.
    ... and you must use the plus function to do so!
-}
sum3 :: Integer -> Integer -> Integer -> Integer
sum3 arg1 arg2 arg3 = _YOUR_CODE_HERE


{-
    Exercise:
    Define isDollar that takes a Char and returns
    True only if that character is a dollar sign ($).
-}
isDollar :: Char -> Bool
isDollar character = _YOUR_CODE_HERE


{-
    Exercise:
    Define an "exclusive or" function: http://en.wikipedia.org/wiki/Exclusive_or#Truth_table
    You probably want to use the following functions:

    (&&) :: Bool -> Bool -> Bool
    (||) :: Bool -> Bool -> Bool
-}
xor :: Bool -> Bool -> Bool
xor arg1 arg2 = _YOUR_CODE_HERE

_YOUR_CODE_HERE = undefined -- ignore me
