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

A `Maybe a` wrapped in any other monad, i.e. `m (Maybe a)

### ReaderT

A `Reader r a` in which the resulting `a` is wrapped in any other monad, i.e. `r -> m a`

### StateT

A `State s a` in which the return value `a` and state `s` are wrapped in any other monad, i.e. `s -> m (a, s)`

### EitherT

An `Either e a` wrapped in any other monad, i.e. `m (Either e a)`

* Simple examples of usage
* Pitfalls of Writer laziness
* Dealing with exceptions and control structures (monad-control and exceptions packages), and losing state
* Monad transformers: [EitherT vs IO](http://stackoverflow.com/questions/25752900/exceptions-and-monad-transformers/25753497#25753497)
* https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md (need to get permission to relicense)
