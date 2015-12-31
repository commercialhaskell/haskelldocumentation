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

[transformers](https://www.stackage.org/package/transformers) is a widely used package which provides transformer versions of various monads. It also provides two useful classes, `MonadTrans` and `MonadIO`. 

`MonadTrans` makes it easy to embed one monad into another. All of the transformers defined in the `transformers` package are instances `MonadTrans`.  

### MonadTrans

`MonadTrans` defines one method, `lift`, the signature of which is 

```haskell
lift :: Monad m => m a -> t m a
```

Given a monad `m`, we can "lift" into a constructed monad transformer `t` so long as `t` is an instance of `MonadTrans`

### MonadIO

`MonadIO` defines one method, `liftIO`, the signature of which is 

```haskell
liftIO :: IO a -> m a
```

`liftIO` allows us to lift an IO action into a transformer stack that is built on top of IO and it works no matter how deeply nested the stack is. We'll see some examples of this below. 


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

Note that in this particular example, the use of `lift` in `lift getLine` is equivalent to `liftIO getLine` since we have a one-layer transformer on top of `IO`.

Here's a (somewhat contrived) example that demonstrates the difference between `lift` and `liftIO` and the usefulness of the latter.

```haskell
getPassword' :: MaybeT (ExceptT MyPasswordError IO) String 
getPassword'  = do 
  password <- liftIO getLine
  guard (isValid password)
  return password
```

In this variation we have more than one layer on top of `IO`. We have a `MaybeT` on top of `ExceptT` on top of `IO`. This is where `liftIO` helps us. We can use use `liftIO` to lift the `getLine` action into our stack no matter how deep `IO` is in our stack. 

If we tried to use `lift` instead of `liftIO`, we'd see the following error:

```
Couldn't match type ‘IO’ with ‘ExceptT MyPasswordError IO’
Expected type: ExceptT MyPasswordError IO String
  Actual type: IO String
In the first argument of ‘lift’, namely ‘getLine’
In a stmt of a 'do' block: password <- lift getLine
```

The error means we have another layer of our stack that we need to traverse before we can lift the IO action into our stack. In other words, we would need to do `lift (lift getLine)`. This is precisely what `liftIO` gives us. Doing `lift . lift . lift ...` is unmaintainable because it relies on the stack being a specific depth. If we decided to add another monad to our stack, our nested lifting would break. With `liftIO` we can short circuit this and simply lift the IO action all the way to the bottom of our stack.


* More transformer usage examples
* Pitfalls of Writer laziness
* Dealing with exceptions and control structures (monad-control and exceptions packages), and losing state
* Monad transformers: [EitherT vs IO](http://stackoverflow.com/questions/25752900/exceptions-and-monad-transformers/25753497#25753497)
* https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md (need to get permission to relicense)
