---
title: Covariance, contravariance, and positive and negative position
author: Michael Snoyman <michael@snoyman.com>
description: Some common terms from category theory that are likely unfamiliar to those with an engineering background.
first-written: 2015-10-20
last-updated: 2015-10-20
last-reviewed: 2015-10-20
---

__NOTE__ This article is still awaiting review for correctness.

Typeclasses such as
[Bifunctor](http://haddock.stackage.org/lts-3.10/base-4.8.1.0/Data-Bifunctor.html)
are often expressed in terms of whether they are *covariant* or
*contravariant*. While these terms may appear intimidating to the unfamiliar,
they are a precise language for discussing these concepts, and once explained
are relatively easy to understand. Furthermore, the related topics of *positive
and negative position* can greatly simplify how you think about complex data
structures. This topic also naturally leads into *subtyping*.

This article is intended to give a developer-focused explanation of the terms
without diving into the category theory behind them too much. For more
information, please see [the Wikipedia page on covariance and
contravariance](https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29).

## The Functor typeclass: covariant functor

Let's consider the following functions (made monomorphic for clarity):

```haskell
showInt :: Int -> String
showInt = show

floorInt :: Double -> Int
floorInt = floor
```

Now suppose that we have a value:

```haskell
maybeInt :: Maybe Int
maybeInt = Just 5
```

We know `Maybe` is an instance of `Functor`, providing us with the following function:

```haskell
fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe = fmap
```

We can use `fmapMaybe` and `showInt` together to get a new, valid, well-typed value:

```haskell
maybeString :: Maybe String
maybeString = fmapMaybe showInt maybeInt
```

However, we can't do the same thing with `floorInt`. The reason for this is
relatively straightforward: in order to use `fmapMaybe` on our `Maybe Int`, we
need to provide a function that takes an `Int` as an input, whereas `floorInt`
returns an `Int` as an output. This is a long-winded way of saying that `Maybe`
is covariant on its type argument, or that the `Functor` typeclass is a
covariant functor.

Doesn't make sense yet? Don't worry, it shouldn't. In order to understand this
better, let's contrast it with something different.

## A non-covariant data type

Consider the following data structure representing how to create a `String`
from something:

```haskell
newtype MakeString a = MakeString { makeString :: a -> String }
```

We can use this to convert an `Int` into a `String`:

```haskell
newtype MakeString a = MakeString { makeString :: a -> String }

showInt :: MakeString Int
showInt = MakeString show

main :: IO ()
main = putStrLn $ makeString showInt 5
```

The output for this program is, as expected, `5`. But suppose we want to both
add `3` to the `Int` and turn it into a `String`. We can do:

```haskell
newtype MakeString a = MakeString { makeString :: a -> String }

plus3ShowInt :: MakeString Int
plus3ShowInt = MakeString (show . (+ 3))

main :: IO ()
main = putStrLn $ makeString plus3ShowInt 5
```

But this approach is quite non-compositional. We'd ideally like to be able to
just apply more functions to this data structure. Let's first write that up
without any typeclasses:

```haskell
newtype MakeString a = MakeString { makeString :: a -> String }

mapMakeString :: (b -> a) -> MakeString a -> MakeString b
mapMakeString f (MakeString g) = MakeString (g . f)

showInt :: MakeString Int
showInt = MakeString show

plus3ShowInt :: MakeString Int
plus3ShowInt = mapMakeString (+ 3) showInt

main :: IO ()
main = putStrLn $ makeString plus3ShowInt 5
```

But this kind of mapping inside a data structure is exactly what we use the
`Functor` type class for, right? So let's try to write an instance!

```haskell
instance Functor MakeString where
    fmap f (MakeString g) = MakeString (g . f)
```

Unfortunately, this doesn't work:

```
Main.hs:4:45:
    Couldn't match type ‘b’ with ‘a’
      ‘b’ is a rigid type variable bound by
          the type signature for
            fmap :: (a -> b) -> MakeString a -> MakeString b
          at Main.hs:4:5
      ‘a’ is a rigid type variable bound by
          the type signature for
            fmap :: (a -> b) -> MakeString a -> MakeString b
          at Main.hs:4:5
    Expected type: b -> a
      Actual type: a -> b
    Relevant bindings include
      g :: a -> String (bound at Main.hs:4:24)
      f :: a -> b (bound at Main.hs:4:10)
      fmap :: (a -> b) -> MakeString a -> MakeString b
        (bound at Main.hs:4:5)
    In the second argument of ‘(.)’, namely ‘f’
    In the first argument of ‘MakeString’, namely ‘(g . f)’
```

To understand why, let's compare the type for `fmap` (specialized to
`MakeString`) with our `mapMakeString` type:

```haskell
mapMakeString :: (b -> a) -> MakeString a -> MakeString b
fmap          :: (a -> b) -> MakeString a -> MakeString b
```

Notice that `fmap` has the usual `a -> b` parameter, whereas `mapMakeString`
instead has a `b -> a`, which goes in the opposite direction. More on that
next.

__Exercise__: Convince yourself that the `mapMakeString` function has the only
valid type signature we could apply to it, and that the implementation is the
only valid implementation of that signature. (It's true that you can change the
variable names around to cheat and make the first parameter `a -> b`, but then
you'd also have to modify the rest of the type signature.)

## Contravariance

What we just saw is that `fmap` takes a function from `a -> b`, and lifts it to
`f a -> f b`. Notice that the `a` is always the "input" in both cases, whereas
the `b` is the "output" in both cases. By contrast, `mapMakeString` has the
normal `f a -> f b`, but the initial function has its types reversed: `b -> a`.
This is the core of covariance vs contravariance:

* In covariance, both the original and lifted functions point in the same
  direction (from `a` to `b`)
* In contravariance, the original and lifted functions point in *opposite*
  directions (one goes from `a` to `b`, the other from `b` to `a`)

This is what is meant when we refer to the normal `Functor` typeclass in
Haskell as a covariant functor. And as you can probably guess, we can just as
easily define a contravariant functor. In fact, [it exists in the contravariant
package](http://haddock.stackage.org/lts-3.10/contravariant-1.3.3/Data-Functor-Contravariant.html#t:Contravariant).
Let's go ahead and use that typeclass in our toy example:

```haskell
import Data.Functor.Contravariant

newtype MakeString a = MakeString { makeString :: a -> String }

instance Contravariant MakeString where
    contramap f (MakeString g) = MakeString (g . f)

showInt :: MakeString Int
showInt = MakeString show

plus3ShowInt :: MakeString Int
plus3ShowInt = contramap (+ 3) showInt

main :: IO ()
main = putStrLn $ makeString plus3ShowInt 5
```

Our implementation of `contramap` is identical to the `mapMakeString` used
before, which hopefully isn't too surprising.

### Example: filtering with `Predicate`

Let's say we want to print out all of the numbers from 1 to 10, where the
English word for that number is more than three characters long. Using a simple
helper function `english :: Int -> String` and `filter, this is pretty simple:

```
greaterThanThree :: Int -> Bool
greaterThanThree = (> 3)

lengthGTThree :: [a] -> Bool
lengthGTThree = greaterThanThree . length

englishGTThree :: Int -> Bool
englishGTThree = lengthGTThree . english

english :: Int -> String
english 1 = "one"
english 2 = "two"
english 3 = "three"
english 4 = "four"
english 5 = "five"
english 6 = "six"
english 7 = "seven"
english 8 = "eight"
english 9 = "nine"
english 10 = "ten"

main :: IO ()
main = print $ filter englishGTThree [1..10]
```

The contravariant package provides a newtype wrapper around such `a -> Bool`
functions, called `Predicate`. We can use this newtype to wrap up our helper
functions and avoid explicit function composition:

```haskell
import Data.Functor.Contravariant

greaterThanThree :: Predicate Int
greaterThanThree = Predicate (> 3)

lengthGTThree :: Predicate [a]
lengthGTThree = contramap length greaterThanThree

englishGTThree :: Predicate Int
englishGTThree = contramap english lengthGTThree

english :: Int -> String
english 1 = "one"
english 2 = "two"
english 3 = "three"
english 4 = "four"
english 5 = "five"
english 6 = "six"
english 7 = "seven"
english 8 = "eight"
english 9 = "nine"
english 10 = "ten"

main :: IO ()
main = print $ filter (getPredicate englishGTThree) [1..10]
```

__NOTE__: I'm not actually recommending this as a better practice than the
original, simpler version. This is just to demonstrate the capability of the
abstraction.

## Bifunctor and Profunctor

We're now ready to look at something a bit more complicated. Consider the
following two typeclasses:
[Profunctor](http://haddock.stackage.org/lts-3.10/profunctors-5.1.1/Data-Profunctor.html)
and
[Bifunctor](http://haddock.stackage.org/lts-3.10/base-4.8.1.0/Data-Bifunctor.html).
Both of these typeclasses apply to types of kind `* -> * -> *`, also known as
"a type constructor that takes two arguments." But let's look at their
(simplified) definitions:

```haskell
class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

class Profunctor p where
    dimap :: (b -> a) -> (c -> d) -> p a c -> p b d
```

They're identical, except that `bimap` takes a first parameter of type `a ->
b`, whereas `dimap` takes a first parameter of type `b -> a`. Based on this
observation, and what we've learned previously, we can now understand the
documentation for these two typeclasses:

> `Bifunctor`: Intuitively it is a bifunctor where both the first and second
> arguments are covariant.
>
> `Profunctor`: Intuitively it is a bifunctor where the first argument is
> contravariant and the second argument is covariant.

These are both bifunctors since they take two type parameters. They both treat
their second parameter in the same way: covariantly. However, the first
parameter is treated differently by the two: `Bifunctor` is covariant, and
`Profunctor` is contravariant.

__Exercise__ Try to think of a few common datatypes in Haskell that would be
either a `Bifunctor` or `Profunctor`, and write the instance.

__Hint__ Some examples are `Either`, `(,)`, and `->` (a normal function from
`a` to `b`). Figure out which is a `Bifunctor` and which is a `Profunctor`.

__Solution__

```haskell
class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

class Profunctor p where
    dimap :: (b -> a) -> (c -> d) -> p a c -> p b d


instance Bifunctor Either where
    bimap f _ (Left x) = Left (f x)
    bimap _ f (Right x) = Right (f x)
instance Bifunctor (,) where
    bimap f g (x, y) = (f x, g y)

instance Profunctor (->) where -- functions
    dimap f g h = g . h . f
```

Make sure you understand *why* these instances work the way they do before
moving on.

## Bivariant and invariant

There are two more special cases for variance: bivariant means "both covariant
and contravariant," whereas invariant means "neither covariant nor
contravariant." The only types which can be bivariant are *phantoms*, where the
type doesn't actually exist. As an example:

```haskell
import Data.Functor.Contravariant (Contravariant (..))

data Phantom a = Phantom
instance Functor Phantom where
    fmap _ Phantom = Phantom
instance Contravariant Phantom where
    contramap _ Phantom = Phantom
```

Invariance will usually (always?) occur when a type parameter is used multiple
times in the data structure, e.g.:

```haskell
data ToFrom a = ToFrom (a -> Int) (Int -> a)
```

__Exercise__ Convince yourself that you can not make an instance of either `Functor` nor `Contravariant` for this datatype.

__Exercise__ Explain why there's also no way to make an instance of `Bifunctor` or `Profunctor` for this datatype.

As you can see, the `a` parameter is used as both the input to a function and
output from a function in the above data type. This leads directly to our next
set of terms.

## Positive and negative position

Let's look at some basic covariant and contravariant data types:

```haskell
data WithInt a = UsesInt (Int -> a)
data MakeInt a = MakeInt (a -> Int)
```

By now, you should hopefully be able to identify that `WithInt` is covariant on
its type parameter `a`, whereas `MakeInt` is contravariant. Please make sure
you're confident of that fact, and that you know what the relevant `Functor`
and `Contravariant` instance will be.

Can we give a simple explanation of why each of these is covariant and
contravariant? Fortunately, yes: it has to do with the position the type
variable appears in the function. In fact, we can even get GHC to tell us this
by using `Functor` deriving:

```haskell
{-# LANGUAGE DeriveFunctor #-}

data MakeInt a = MakeInt (a -> Int)
    deriving Functor
```

This results in the (actually quite readable) error message:

```
Can't make a derived instance of ‘Functor MakeInt’:
  Constructor ‘MakeInt’ must not use the type variable in a function argument
In the data declaration for ‘MakeInt’
```

Another way to say this is "`a` appears as an input to the function." An even
better way to say this is that "`a` appears in negative position." And now we
get to define two new terms:

* Positive position: the type variable is the result/output/range/codomain of the function
* Negative position: the type variable is the argument/input/domain of the function

When a type variable appears in positive position, the data type is covariant
with that variable. When the variable appears in negative position, the data
type is contravariant with that variable. To convince yourself that this is
true, go review the various data types we've used above, and see if this logic
applies.

But why use the terms positive and negative? This is where things get quite
powerful, and drastically simplify your life. Consider the following newtype
wrapper intended for callbacks:

```haskell
newtype Callback a = Callback ((a -> IO ()) -> IO ())
```

Is it covariant or contravariant on `a`? Your first instinct may be to say
"well, `a` is a function parameter, and therefore it's contravariant. However,
let's break things down a bit further.

Suppose we're just trying to deal with `a -> IO ()`. As we've established many
times above: this function is contravariant on `a`, and equivalently `a` is in
negative position. This means that this function expects on input of type `a`.

But now, we wrap up this entire function as the input to a new function, via:
`(a -> IO ()) -> IO ()`. As a whole, does this function *consume* an `Int`, or
does it *produce* an `Int`? To get an intuition, let's look at an
implementation of `Callback Int` for random numbers:

```haskell
supplyRandom :: Callback Int
supplyRandom = Callback $ \f -> do
    int <- randomRIO (1, 10)
    f int
```

It's clear from this implementation that `supplyRandom` is, in fact,
*producing* an `Int`. This is similar to `Maybe`, meaning we have a solid
argument for this also being covariant. So let's go back to our
positive/negative terminology and see if it explains why.

In `a -> IO ()`, `a` is in negative position. In `(a -> IO ()) -> IO ()`, `a ->
IO ()` is in negative position. Now we just follow multiplication rules: when
you multiply two negatives, you get a positive. As a result, in `(a -> IO ())
-> IO ()`, `a` is in positive position, meaning that `Callback` is covariant on
`a`, and we can define a `Functor` instance. And in fact, GHC agrees with us:

```haskell
{-# LANGUAGE DeriveFunctor #-}
import System.Random

newtype Callback a = Callback
    { runCallback :: (a -> IO ()) -> IO ()
    }
    deriving Functor

supplyRandom :: Callback Int
supplyRandom = Callback $ \f -> do
    int <- randomRIO (1, 10)
    f int

main :: IO ()
main = runCallback supplyRandom print
```

Let's unwrap the magic, though, and define our `Functor` instance explicitly:

```haskell
newtype Callback a = Callback
    { runCallback :: (a -> IO ()) -> IO ()
    }

instance Functor Callback where
    fmap f (Callback g) = Callback $ \h -> g (h . f)
```

__Exercise 1__: Analyze the above `Functor` instance and understand what is occurring.

__Exercise 2__: Convince yourself that the above implementation is the only one
that makes sense, and similarly that there is no valid `Contravariant`
instance.

__Exercise 3__: For each of the following newtype wrappers, determine if they
are covaraint or contravaraint in their arguments:

```haskell
newtype E1 a = E1 (a -> ())
newtype E2 a = E2 (a -> () -> ())
newtype E3 a = E3 ((a -> ()) -> ())
newtype E4 a = E4 ((a -> () -> ()) -> ())
newtype E5 a = E5 ((() -> () -> a) -> ())

-- trickier:
newtype E6 a = E6 ((() -> a -> a) -> ())
newtype E7 a = E7 ((() -> () -> a) -> a)
newtype E8 a = E8 ((() -> a -> ()) -> a)
newtype E9 a = E8 ((() -> () -> ()) -> ())
```
