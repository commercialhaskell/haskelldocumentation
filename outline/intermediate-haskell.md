---
title: Intermediate Haskell
author: Michael Snoyman <michael@fpcomplete.com>
description: Material to guide a Haskell beginner to becoming a Haskell expert
first-written: 2015-02-24
last-updated: 2015-02-24
last-reviewed: 2015-02-24
---

This outline provides a wide array of content, focused on practical lessons
towards writing real-world applications. It presumes a basic knowledge of
Haskell, as would be gained from books such as Real World Haskell and Learn You
a Haskell.

Much of the content described below does not yet exist, and therefore
contributions are highly welcome. Additionally, some of the lists below should
be expanded. If you have thoughts on missing pieces, please bring them up on
the issue tracker.

## Core information

You understand the basics of Haskell syntax and some common library functions.
This section should get you up to speed with many commonly used features of
Haskell, to provide a foundation for understanding code in general, and to
follow the rest of this outline in particular.

* [Basic Tooling Guide](../content/basic-tooling-guide.md)
* Tooling & IDEs
* [Common Typeclasses](../content/common-typeclasses.md)
* [Common Language Extensions](../content/common-language-extensions.md)
* [Haskell glossary](../content/haskell-glossary.md)
* [All About Exceptions](../content/all-about-exceptions.md)
* Basics of lazy evaluation

## Data structures

Covers some of the most commonly used data structures in Haskell, and the
libraries providing them.

* vector (cover vector-algorithms)
* containers
* unordered-containers
* text (cover text-icu)
* bytestring

## General patterns

This section demonstrates some common Haskell coding patterns, how they work,
when they're useful, and possible pitfalls.

* [Monad Transformers](../content/monad-transformers.md)
* Continuation Passing Style
* Builders and difference lists

## Testing

* QuickCheck
* hspec, tasty, others?

## Serialization

* binary/cereal
* blaze-builder/bytestring-builder
* blaze-html
* attoparsec
* aeson
* yaml
* xml-conduit/html-conduit
* base16-bytestring/base64-bytestring

## Standard programming needs

* HTTP client library
* Command line argument parsing optparse-applicative
* cryptohash
* time
* Random number generation (mwc-random)
* Possibly others from: https://www.fpcomplete.com/school/using-fphc/recommended-libraries
* Regular expressions with regex-applicative

## System programming

* Launching subprocesses, capturing output, working with environment (can include [Data.Conduit.Process](https://www.fpcomplete.com/user/snoyberg/library-documentation/data-conduit-process))
* Network and Socket I/O
* Writing scripts (turtle, Shelly)

## Best practices

* [Exceptions best practices](../content/exceptions-best-practices.md)
* Typeclasses versus records
* "Good" use of typeclass extensions
* Proper error reporting (Either, Maybe, ErrorT, exceptions package and using MonadThrow)

## Streaming data

Streaming data libraries allow you to process large amounts of input with
reliable resource usage, be that memory, file descriptors, or other resources.
There are a number of different libraries for doing this in Haskell. Instead of
a single library, each library can have its own subsection here. In addition,
the following provides an overview of the different options.

* [Overview of Streaming Data Libraries](../content/overview-streaming-data-libraries.md)

### conduit

* https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview
* https://www.fpcomplete.com/user/snoyberg/library-documentation/resourcet
* http://www.yesodweb.com/blog/2014/03/network-conduit-async
* https://www.fpcomplete.com/user/snoyberg/library-documentation/vectorbuilder
* conduit-combinators

## Concurrency and parallelism

Simon Marlow's book [Parallel and Concurrent Programming in
Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html) is a
highly recommended read on the subject. In addition, we have the following
topics:

* The async package
* Common concurrency patterns (e.g., the auto-update package)
* Concurrency patterns: worker threads, signals, blocking on TVars
* STM: blocking semantics around mutable variables
* resource-pool
* handling errors (SlaveThread), restarting tasks, timeouts and other common patterns

## Web programming

Web programming is another topic with many different approaches. Like streaming
data, we need an overview of the different options, and then a drilldown on
individual approaches. For now:

* [Web Application Interface](https://github.com/yesodweb/yesodweb.com-content/blob/master/book/asciidoc/web-application-interface.asciidoc)
* [Yesod Web Framework](http://www.yesodweb.com/book)

## Big library guide

The following libraries are somewhat "large" in the sense that they address
many different concerns.

* lens
* mono-traversable

### Alternate Preludes

Sometimes it is useful to use an alternative to the standard `Prelude`. Reasons
include avoiding cross-version incompatibility, support for better data
structures, and avoiding partial functions. Here are some commonly used
preludes (in alphabetical order).

* base-prelude
* basic-prelude
* classy-prelude

## Advanced topics

* [Primitive Haskell](../content/primitive-haskell.md)
* https://wiki.haskell.org/Evaluation_order_and_state_tokens
* Cabal trickery for backwards compatibility: Cabal CPP macros. Paths module. Flags. How to test for windows. Defaulting macros for ghci. Flags to either use new library version or another package (bytestring-builder) and set a CPP variable.

## Database Programming

* persistent
* esqueleto
* opaleye
* mysql-simple
* postgresql-simple

## Debugging/optimizing

* hlint
* Debugging
* Profiling
* Finding space leaks
* Strictness annotations
* Pragmas (UNPACK, INLINE, ...)
* Heap profiling
* Looking at GHC core

## Code and project structuring

As a project grows, there are many "patterns" that might save developer some time by just doing some restructuring work. Some tricks might save development time, while others help to re-compile less.

* Common `Imports.hs` module
* Multiple executables depending on common library
