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

### ExceptT

An `Either e a` wrapped in any other monad, i.e. `m (Either e a)`

## Simple examples of usage

[transformers](https://www.stackage.org/package/transformers) is a widely used package which provides transformer versions of various monads. It also provides two useful classes, `MonadTrans` and `MonadIO`. 

Instances of `MonadTrans` are transformers which can be applied to other monads to create new monads. All of the transformers defined in the `transformers` package are instances of `MonadTrans`.  

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

### lift vs liftIO

Here's a (somewhat contrived) example that demonstrates the difference between `lift` and `liftIO` and the usefulness of the latter.

Suppose we added another layer to our transformer stack so that, instead of `MaybeT IO String`, we had `MaybeT (ExceptT MyPasswordError IO) String`. 

As in our first example, we'd like to lift the `getLine` action into our transformer. Let's try.

```haskell
getPassword' :: MaybeT (ExceptT MyPasswordError IO) String 
getPassword'  = do 
  password <- lift getLine
  guard (isValid password)
  return password
```

We get an error. Oops!

```
Couldn't match type ‘IO’ with ‘ExceptT MyPasswordError IO’
Expected type: ExceptT MyPasswordError IO String
  Actual type: IO String
In the first argument of ‘lift’, namely ‘getLine’
In a stmt of a 'do' block: password <- lift getLine
```

If we look at the type of `lift` when specialized to various transformers, we can see the problem.

```
> :t \x -> (lift x :: MaybeT IO String)
\x -> (lift x :: MaybeT IO String) :: IO String -> MaybeT IO String
``` 

In this example, we can use `lift` to go from `IO` into our transformer. But with a deeper stack, we run into problems:

```> type MyDeeperStack = ReaderT Int (WriterT String IO) Bool
> :t \x -> (lift x :: MyDeeperStack)
\x -> (lift x :: MyDeeperStack)
  :: WriterT String IO Bool -> MyDeeperStack
```

In other words, the `m` from `lift :: m a -> t m a` in our `MyDeeperStack` is `WriterT String IO`. So we would to need `lift` *again* in order to go from `IO Bool -> MyDeeperStack`, i.e.

```> :t \x -> ((lift . lift) x :: MyDeeperStack)
\x -> ((lift . lift) x :: MyDeeperStack)
  :: IO Bool -> MyDeeperStack
```

This is where `liftIO` helps us. It essentially lets us do a variable number of lifts. This lets us write less brittle code because if we decided to add yet another layer to our transformer stack, we wouldn't have to hardcode another call to `lift`. 

As an example, what happens if we add a `MaybeT` to our stack?

```haskell
type MyDeeperStack = ReaderT Int (WriterT String (MaybeT IO)) Bool
```

`lift . lift` will no longer allow us to lift an `IO` action into our stack because we now have a third layer. 

```> :t \x -> ((lift . lift) x :: MyDeeperStack)
\x -> ((lift . lift) x :: MyDeeperStack)
  :: MaybeT IO Bool -> MyDeeperStack
```

With `liftIO`, as is well:

```> :t \x -> (liftIO x :: MyDeeperStack)
\x -> (liftIO x :: MyDeeperStack) :: IO Bool -> MyDeeperStack
```

Want to add another layer? No problem:

```haskell
type MyDeeperStack = ReaderT Int (WriterT String (MaybeT (ExceptT String IO))) Bool
```

```> :t \x -> (liftIO x :: MyDeeperStack)
\x -> (liftIO x :: MyDeeperStack) :: IO Bool -> MyDeeperStack
```

Without `liftIO` we'd need to keep adjusting the number of lifts:

```> :t \x -> ((lift . lift . lift . lift)  x :: MyDeeperStack)
\x -> ((lift . lift . lift . lift)  x :: MyDeeperStack)
  :: IO Bool -> MyDeeperStack
```

* More transformer usage examples
* Pitfalls of Writer laziness
* Dealing with exceptions and control structures (monad-control and exceptions packages), and losing state
* Monad transformers: [EitherT vs IO](http://stackoverflow.com/questions/25752900/exceptions-and-monad-transformers/25753497#25753497)
* https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md (need to get permission to relicense)
