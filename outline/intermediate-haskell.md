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

__NOTE__ This list was copy-pasted from MezzoHaskell, and needs to be
restructured correctly into outline format.

## "Core"

* Exception handling
* Asynchronous exceptions
* [Exceptions best practices](../content/exceptions-best-practices.md)
* Basic typeclasses (Monoid, Applicative, Alternative)

## Common techniques

* Monad transformers
    * monad-control
* CPS

## Language extensions

* OverloadedStrings
* ViewPatterns
* PatternGuards
* TypeFamilies
* FunDeps
* MPTC
* GADT
* TemplateHaskell
* QuasiQuotes

## Data structures

* vector
* containers
* unordered-containers
* text
* bytestring

## Serialization

* binary/cereal
* blaze-builder
* blaze-html
* attoparsec
* aeson
* yaml
* xml-conduit

## Other libraries

* system-filepath
* esqueleto

## Open debates

* Streaming data
    * conduit
    * iteratee/enumerator
    * pipes
* Typeclasses versus records
* "Good" use of typeclass extensions
* Proper error reporting (Either, Maybe, ErrorT)

## Tools

* cabal
* Test framework

## Debugging/optimizing

* hlint
* Debugging
* Profiling
* Finding space leaks
* Strictness annotations
* Pragmas (UNPACK, INLINE, ...)
* Heap profiling
* Looking at GHC core

## Misc topics (not sorted in yet, just added now)

* Builders
* Monad transformers: [EitherT vs IO](http://stackoverflow.com/questions/25752900/exceptions-and-monad-transformers/25753497#25753497)
* [Wrap exceptions to provide context](http://stackoverflow.com/questions/27346380/how-to-wrap-exceptions-to-provide-context)
* [General dislike of exceptions](http://www.reddit.com/r/haskell/comments/2ety9f/new_blog_post_dealing_with_asynchronous/ck3fkbp)
* STM: blocking semantics around mutable variables
* The async package
* exceptions package and using MonadThrow
* Tutorial on Vector
* Concurrency patterns: worker threads, signals, blocking on TVars
* Cabal CPP macros. Paths module. Flags. How to test for windows. Defaulting macros for ghci. Flags to either use new library version or another package (bytestring-builder) and set a CPP variable.
* Exceptions problems. Can't wrap. Can't have two exceptions. No idea how exception was thrown.
* Proper way to call external programs
* Haskell glossary. Define commonly used but not-commonly-understood terms (example: covariant, contravaraint, positive position, negative position)
* [Primitive Haskell](../content/primitive-haskell.md)

## External content worth importing:

This is starting off as a biased list of my own content. Others should feel free to add to it themselves.

* Everything from: https://www.fpcomplete.com/user/snoyberg/library-documentation, especially once we have export-to-SoH functionality
* https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers
* https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
* https://www.fpcomplete.com/user/snoyberg/general-haskell/basics/functors-applicative-functors-and-monads
* https://github.com/yesodweb/yesodweb.com-content/blob/master/book/asciidoc/web-application-interface.asciidoc
* http://www.yesodweb.com/blog/2014/09/woes-multiple-package-versions
* http://www.yesodweb.com/blog/2014/05/exceptions-cont-monads
* http://www.yesodweb.com/blog/2014/03/network-conduit-async

Stuff from Haskell Wiki?

* https://wiki.haskell.org/Evaluation_order_and_state_tokens
