---
title: Monad Transformers
author: Michael Snoyman <michael@fpcomplete.com>
description: What transformers are, why they're useful, what to be aware of
first-written: 2015-02-24
last-updated: 2015-02-24
last-reviewed: 2015-02-24
---

# Monad Transformers

## Basic Transformers 

The following is a list of some basic transformers:

### MaybeT

A `Maybe a` wrapped in any other monad, i.e. `m (Maybe a)`

### ReaderT

A `Reader r a` in which the resulting `a` is wrapped in any other monad, i.e. `r -> m a`

### StateT

A `State s a` in which the return value and state, namely `(a, s)`, are wrapped in any other monad, i.e. `s -> m (a, s)`

### EitherT

An `Either e a` wrapped in any other monad, i.e. `m (Either e a)`

## Simple examples of usage

### MonadTrans

[transformers](https://www.stackage.org/lts-3.20/package/transformers-0.4.2.0) is a widely used package which provides transformer versions of various monads. 

It also provides a `MonadTrans` class which makes it easy to embed one monad into another. All of the transformers defined in the `transformers` package are instances `MonadTrans`.  

`MonadTrans` defines one method, `lift`, the signature of which is 

```haskell
lift :: Monad m => m a -> t m a
```

Given a monad `m`, we can "lift" into a constructed monad transformer `t` so long as `t` is an instance of `MonadTrans`

Examples:

### MaybeT 

```haskell
import Control.Monad 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Class 

main = do 
  password <- runMaybeT getPassword
  case password of 
    Just p  -> putStrLn "valid password!"
    Nothing -> putStrLn "invalid password!"

isValid :: String -> Bool
isValid = (>= 10) . length

getPassword :: MaybeT IO String 
getPassword = do 
  password <- lift getLine
  guard (isValid password)
  return password 
```

In this example, we combine the `IO` and `Maybe` monads. `lift getLine` allows us to embed the `IO` action into the `MaybeT` transformer, yielding a value of type `MaybeT IO String`.  


* More transformer usage examples
* Pitfalls of Writer laziness
* Dealing with exceptions and control structures (monad-control and exceptions packages), and losing state
* Monad transformers: [EitherT vs IO](http://stackoverflow.com/questions/25752900/exceptions-and-monad-transformers/25753497#25753497)
* https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md (need to get permission to relicense)
