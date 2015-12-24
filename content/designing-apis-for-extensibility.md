---
title: Designing APIs for Extensibility
author: Michael Snoyman <michael@fpcomplete.com>
description: Guidelines on making your APIs stable and extensible
first-written: 2015-03-08
last-updated: 2015-03-08
last-reviewed: 2015-03-08
---

Every time you make a breaking change in your API, it means that- potentially-
one or more of your users will need to change his/her code to adapt. Even if
this update is trivial, it adds friction to the code maintenance process. On
the other hand, we don't want to be constrained by bad design choices early on
in a project, and sometimes a breaking API change is the best option.

The point of this document, however, is to give you a third option: design your
APIs from the outset to be extensible. There are common techniques employed in
the Haskell world to make APIs that are resilient to changing feature-sets, and
by employing them early on in your design process, you can hopefully avoid the
painful choices between a better API and happy users.

Almost all techniques start with implementation hiding. Guidelines here are
simple: don't expose anything non-public. For example, if you write a number of
helper functions, you may not want to start off by exposing them, since you're
then telling users that these are good, stable functions to be relied upon.
Instead, use explicit export lists on your modules and only include functions
that are intended for public consumption.

More important- and more tricky- than functions are data constructors. In many
cases, you want to avoid exposing the internals of your data types to users, to
allow you to expand on them in the future. A common use case for this is some
kind of a data type providing configuration information. Consider that you're
going to communicate with some web services, so you write up the following API:

```haskell
module MyAPI
    ( Settings (..)
    , makeAPICall
    ) where

data Settings = Settings
    { apiKey :: Text
    , hostName :: Text
    }

makeAPICall :: Settings -> Foo -> IO Bar
```

The way your users will access this will be something like:

```haskell
makeAPICall Settings
    { apiKey = myAPIKey
    , hostName = "www.example.com"
    } myFoo
```

Now suppose a user points out that, in some cases, the standard port 80 is
*not* used for the API call. So you add a new field `port :: Int` to your
`Settings` constructor. This will break your user's code, since the `port`
field will not be set.

Instead, a more robust way of specifying your API will look like:

```haskell
module MyAPI
    ( Settings
    , mkSettings
    , setHostName
    , makeAPICall
    ) where

data Settings = Settings
    { apiKey :: Text
    , hostName :: Text
    }

-- | Create a @Settings@ value. Uses default value for host name.
mkSettings :: Text -- ^ API Key
           -> Settings
mkSettings key = Settings
    { apiKey = key
    , hostName = "www.example.com"
    }

setHostName :: Text -> Settings -> Settings
setHostName hn s = s { hostName = hn }

makeAPICall :: Settings -> Foo -> IO Bar
```

Now your user code will instead look like:

```haskell
makeAPICall (mkSettings myAPIKey) myFoo
```

This has the following benefits:

* The user is not bothered to fill in default values (in this case, the hostname).
* Extending this API to allow for more fields in the future is trivial: add a new `set*` function. Internally, you'll add a field to `Settings` and set a default value in `mkSettings`.

One thing to note: please do *not* expose the field accessors directly. If you
want to provide getter functions in addition to setters, write them explicitly,
e.g.:

```haskell
getHostName :: Settings -> Text
getHostName = hostName
```

The reason for this is that by exposing field accessors, users will be able to write code such as:

```haskell
(mkSettings myAPIKey) { hostName = "www.example.org" }
```

This ties your hand for future internal improvements, since you are now
required to keep a field of name `hostName` with type `Text`. By just using
`set` and `get` functions, you can change your internal representation
significantly and still provide a compatibility layer.

For those of you familiar with other languages: this is in fact quite similar
to the approach taken in Java or C#. Just because Java does it doesn't mean
it's wrong.

Note that this advice is different to, and intended to supersede, [the settings
type approach](http://www.yesodweb.com/book/settings-types). Projects like Warp
which previously used that settings type approach are currently migrating to
this more extensible approach.

Also, while settings have been used here as a motivating example, the same
advice applies to other contexts.

## Internal modules

One downside of implementation hiding is that it can make it difficult for
users to do things you didn't intend for them to do with your API. You can
always add more functionality on demand, but the delay can be a major nuissance
for users. A compromise solution in the Haskell community is to provide a
`.Internal` module for your project which exports not-quite-public components.
For example, in wai, the `Response` constructors are exposed in a
`Network.Wai.Internal` module. Normally, users are supposed to use smart
constructors like `responseFile`, but occasionally they'll want more
fine-grained control.
