# Haskell Style

**TL;DR**: If it can be automated, it should be automated. If the automated
outcome has no _objective_ readability problems, we should accept it.

We use three Haskell auto-formatting tools:

- Stylish Haskell (v0.9.2.2)

  We use Stylish Haskell for `import` formatting only, because it does a better
  job than Brittany. The only manual choice we have to make is if/how to group,
  for which we have a simple rule you can find below.

- Brittany (v0.11.0.0)

  Brittany will auto-format almost all Haskell code in an automatic way. The
  current version has some hard edges (hopefully fixed in v0.12), which lead to
  a few valid exceptions:

  - Operator-heavy expressions where it's valuable to maintain a particular
    visual shape for readability. Examples: Esqueleto, optparse-applicative
    parsers, or lens-constructions for Stratosphere.

  - Multi-line quasi-quotes. Examples `aesonQQ`, `hamlet`, etc.

  Such exceptions can be disabled by pragma. Prefer doing this, so the rest of
  the file can still auto-format successfully.

- HLint (v2.1.11)

  HLint, through `apply-refact`, will auto-apply some Hints. We should accept
  these fixes or adjust HLint's configuration to not contain the hint being
  applied.

Throughout the below guide, things we used to have a manual guide on but are now
automated by the above tools are noted as such. This (vs just omitting them) is
done for a few reasons:

- Some choices are so core to a Style Guide, that not having them visible here
  might give the impression we don't actually have a defined choice
- This gives us a chance to see just how much time we've saved by automating
  what used to be a (possibly tenuous) negotiated consensus

## Line Length

**Automated**.

## Comments

Comments should follow
[Haddock style](https://github.com/frontrowed/guides/blob/master/haskell-style.md#haddocks).

## Monad sequences and Haskell arrows

Monadic sequences should normally go in one direction, including `<-` from do
notation

```haskell
-- Bad
result <- m >>= return

-- Good
result <- return =<< m
```

That isn't to say we prefer `=<<` over `>>=`

```haskell
-- Good
result <-
      action4
  =<< action3
  =<< action2
  =<< action1

-- Better
result <-
      action1
  >>= action2
  >>= action3
  >>= action4

-- Good: this is easy to read left-to-right and scales as the lambda grows

m >>= (\x -> )
```

In general left bind does not scale well for inline code. This is ok.

```hs
(\x -> f x) =<< m
```

This is bad.

```hs
(\x -> do f x
          g x
) =<< m
```

The lambda should be turned into a bound function.

## Distinguishing function arguments

Be careful of creating functions that have the same input type

```hs
f :: Int -> Int -> Int -> Int
```

This is a good time to look at using a record to name the arguments or to use
newtypes around the `Int`s. Note that the fact that the output is the same type
as the input is not a concern: the below is fine

```hs
f :: Int -> Int
```

## Data type declarations

### Records

**Automated**.

### Sum Types

**Automated**.

## Function Type Signatures

**Automated**.

## Alignment

**Automated**.

## Imports

**Automated**: formatting of the import statements themselves.

Haskell's modules expose some variety in import style:

- Open imports
- Explicit imports
- Exclusionary imports
- Qualified imports
- Aliased imports

Good style prefers:

- Open imports for common libraries
  - `base`
  - `mtl`
  - custom preludes
- Explicit imports for bringing lesser known functions in to scope
- Exclusionary imports for avoiding minor name clashes
  - `lens`
- Qualified imports for major name clashes
  - `containers`
  - `unordered-containers`
- Qualified imports for "ad-hoc module schema" (see example)
- Aliased imports for packaging and exporting many modules in a single module.
  - creating a custom prelude

```haskell
-- Good
import Control.Lens hiding (at)
import Control.Monad (forever)
import Control.Monad.Logger (logInfoN, logErrorN)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Text as Text

-- Bad
-- Overly open imports lead to increased ambiguity forcing common functions to
-- be qualified.
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Map as Map
import Data.Text as Text

-- Bad
-- Over qualification leads to increased line noise and length.
import qualified Control.Lens as Lens
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Text as Text
```

While we don't prefer writing all modules to assume they'll be qualified, there
are cases where we implement related modules to share an interface of functions.
In such cases, we would use un-qualified naming and expect `qualified` imports:

```hs
-- Bad
import FrontRow.Jobs.SyncTeacher (enqueueSyncTeacher)
import FrontRow.Jobs.DeleteTeacher (enqueueDeleteTeacher)

main = do
  if shouldDeleteTeacher teacher
    then enqueueDeleteTeacher teacher
    then enqueueSyncTeacher teacher

-- Good
import qualified FrontRow.Jobs.SyncTeacher as SyncTeacher
import qualified FrontRow.Jobs.DeleteTeacher as DeleteTeacher

main = do
  if shouldDeleteTeacher teacher
    then DeleteTeacher.enqueue teacher
    then SyncTeacher.enqueue teacher
```

### Importing types and qualifying

For modules that exports a type and (clashing) identifiers for operating on that
type, import the type un-qualified and the rest of the module qualified:

```hs
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
```

### Abbreviating qualifications

**Automated**.

### Import groups

Put one blank line between `module`-`where` and the start of your `import`s. Put
your preferred prelude (when explicit) first, followed by a blank line, then the
rest of your imports.

## Exports

**Automated**: the formatting of `module (...) where` itself.

Sort your exports, unless the order matters in your desired Haddock output.

## Declaring Extensions

**Automated**: sorting and formatting of extension pragmas.

- All Haskell packages MUST use the following `default-extensions`:

  ```yaml
  default-extensions:
    - BangPatterns
    - DataKinds
    - DeriveAnyClass
    - DeriveFoldable
    - DeriveFunctor
    - DeriveGeneric
    - DeriveLift
    - DeriveTraversable
    - DerivingStrategies
    - DerivingVia
    - FlexibleContexts
    - FlexibleInstances
    - GADTs
    - GeneralizedNewtypeDeriving
    - LambdaCase
    - MultiParamTypeClasses
    - NoImplicitPrelude
    - NoMonomorphismRestriction
    - OverloadedStrings
    - QuasiQuotes
    - RankNTypes
    - RecordWildCards
    - ScopedTypeVariables
    - StandaloneDeriving
    - TypeApplications
    - TypeFamilies
  ```

  This defines a consistent, and minimally-extended Haskell environment. Other
  extensions MUST be defined via LANGUAGE pragmas in the modules where they're
  needed.

  We allow our `entities` package to diverge from this list, since it is almost
  entirely persistent Entity definitions. Within this package only, we also have
  the following enabled by default:

  - `TemplateHaskell`
  - `UndecidableInstances`

- Leave a blank line after the extensions list

  ```haskell
  {-# LANGAUGE OverloadedStrings #-}

  module Foo
    ( foo
    )
  ```

  ```haskell
  {-# LANGAUGE OverloadedStrings #-}

  -- |
  --
  -- The Foo module does the foo-ing
  --
  module Foo
    ( foo
    )
  ```

## Haddocks

Haskell modules should be commented with valid Haddock documentation. We are not
yet requiring a certain level of coverage, but it is strongly encouraged.

### General guides

- Use proper Haddock [markup][]
- Link all identifiers, anywhere they appear

  Linked identifiers will automatically be monospace, so you don't need to
  `@'DoThis'@`. However, if you have an identifier as part of a larger monospace
  phrase, you will need to `@'Maybe' ('Do', 'This')@`.

  Bad

  ```hs
  -- | Construct a @'FlipFlop'@
  --
  -- If the size is right, you will get a @Right FlipFlop@, otherwise a @Left@
  --
  ```

  Good

  ```hs
  -- | Construct a 'FlipFlop'
  --
  -- If the size is right, you will get a @'Right' 'FlipFlop'@, otherwise a
  -- 'Left'.
  --
  ```

- Use leading documentation (`-- |`) for top-level definitions and trailing
  documentation (`-- ^`) for record attributes and function arguments

  Bad

  ```hs
  data Foo
    = Foo
    {
    -- | Foo's foo
      fooFoo :: Foo
    -- | Foo's bar
    , fooBar :: Bar
    }
  -- ^ A mispelling of fu to avoid detection when coupled with Bar
  ```

  Good

  ```hs
  -- | A mispelling of fu to avoid detection when coupled with Bar
  data Foo
    = Foo
    { fooFoo :: Foo
    -- ^ Foo's foo
    , fooBar :: Bar
    -- ^ Foo's bar
    }
  ```

  Best, for this case

  ```hs
  -- | A mispelling of fu to avoid detection when coupled with Bar
  data Foo
    = Foo
    { fooFoo :: Foo -- ^ Foo's foo
    , fooBar :: Bar -- ^ Foo's bar
    }
  ```

  **NOTE**: do not align trailing documentation in context-sensitive ways.

  Bad

  ```hs
  data Foo
    = Foo
    { fooFoos :: [Foo] -- ^ Foo's foos
    , fooBar :: Bar    -- ^ Foo's bar
    }
  ```

  Good

  ```hs
  data Foo
    = Foo
    { fooFoos :: [Foo] -- ^ Foo's foos
    , fooBar :: Bar -- ^ Foo's bar
    }
  ```

  And apply Summary/Body rules for long trailing documentation.

  Bad

  ```hs
  data Foo
    = Foo
    { fooFoos :: [Foo] -- ^ Foo's foos is getting really long and might be
                       -- multiple sentences. You might want to go
                       -- context-sensitive too!
    , fooBar :: Bar -- ^ Foo's bar
    }
  ```

  Good

  ```hs
  data Foo
    = Foo
    { fooFoos :: [Foo]
    -- ^ Foo's foos
    --
    -- We now need a Body, and all the usual rules apply. It's getting really
    -- long but we aren't context-sensitive and we can easily wrap. Don't forget
    -- the surrounding whitespace!
    --
    , fooBar :: Bar -- ^ Foo's bar
    }
  ```

- Don't use Haddock where it doesn't belong

  Haddock is not great about ignoring its syntax when it doesn't expect it.
  Introducing such cases can case the documentation build to fail.

  Bad

  ```hs
  foo = do
    -- | Here's a note about a thing
    let some = variable

    doTheThing -- ^ Careful here
   where
    -- | And another note
    variable = other
  ```

  Good

  ```hs
  foo = do
    -- Here's a note about a thing
    let some = variable

    doTheThing -- Careful here
   where
    -- And another note
    variable = other
  ```

### Summaries

Summaries must be a single, short (e.g. non-wrapping), capitalized sentence; not
punctuated, and in a declarative tense.

A Summary should complete the sentence:

> This (module|type|attribute|function|argument)... {Summary}

Most rules that would apply to [commit messages][tbaggery] apply here.

Bad

```hs
-- | Be careful here, this is tricky!

-- | Here we're returning the Admins that can access the other thing by virtue
-- of the fact that they are this thing

-- | Does a thing. Is partial because of random reason
```

Good

```hs
-- | Represents a value that may or may not be present

-- | Updates all Admins to @'isVerified' = 'True'@

-- | Returns the head of a non-empty list, or raises an exception
```

When a Body is _not_ present (see below), no newline is required between a
Summary and its associated top-level definition:

Bad

```hs
-- | The worse of the 'Thing' twins
--
badThing :: Thing
badThing = Thing 1
```

Good

```hs
-- | The better of the 'Thing' twins
goodThing :: Thing
goodThing = Thing 2
```

### Body

Bodies are optional but encouraged. When present, the following applies:

- Wrap non-literal content at 80 columns (not our usual 120)
- Surround block elements by a line of whitespace

  Bad

  ```hs
  -- | The docs
  -- Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  -- tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
  -- veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
  -- commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
  -- velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
  -- cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id
  -- est laborum.
  theFunction

  -- | The docs
  --
  -- Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  -- tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
  -- veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
  -- commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
  -- velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
  -- cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id
  -- est laborum.
  theFunction

  -- | The docs
  --
  -- Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  -- tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
  -- veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
  -- commodo consequat.
  -- Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
  -- dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
  -- proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
  theFunction
  ```

  Good

  ```hs
  -- | The docs
  --
  -- Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  -- tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
  -- veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
  -- commodo consequat.
  --
  -- Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
  -- dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
  -- proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
  --
  theFunction
  ```

- Lists receive a hanging indent

  Bad

  ```hs
  -- | The docs
  --
  -- 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  -- tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
  -- veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
  -- commodo consequat.
  -- 2. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
  -- dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
  -- proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
  --
  theFunction
  ```

  Good

  ```hs
  -- | The docs
  --
  -- 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  --    tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
  --    veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
  --    commodo consequat.
  -- 2. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
  --    dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
  --    proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
  --
  theFunction
  ```

### Module organization

- Organize your exports by logical groups or [progressive
  disclosure][progressive_disclosure] and use section headings

  Bad

  ```hs
  module Foo
    ( getFoo
    , updateBar
    , internalDeleteFoo
    , deleteFoo
    )
  ```

  Good

  ```hs
  module Foo
    (
    -- * Foo
      getFoo
    , deleteFoo

    -- * Bar
    , updateBar

    -- * Internal, exported for testing
    , internalDeleteFoo
    )
  ```

- Consider adding section documentation

  **NOTE**: Summary/Body rules apply!

  ```hs
  module Foo
    (
    -- * Foo
    -- | Operates on Foos
      getFoo
    , deleteFoo

    -- * Bar
    -- | Operates on Bars
    , updateBar

    -- * Internal
    -- | Exported for testing only
    --
    -- Do not use these, unstable API.
    --
    , internalDeleteFoo
    )
  ```

- If you want to separate the definitions _in_ the module, use [named
  chunks][chunks].

  ```hs
  module Foo
    (
    -- * Foo
    -- $foo
      getFoo
    , deleteFoo

    -- * Bar
    -- $bar
    , updateBar

    -- * Internal
    -- $internal
    , internalDeleteFoo
    )
  where

  -- $foo
  -- Operates on Foos

  data Foo

  getFoo

  deleteFoo

  -- $bar
  -- Operates on Bars

  data Bar

  updateBar

  -- $internal
  -- Exported for testing only
  --
  -- Do not use these, unstable API.
  --

  data DeleteAction

  internalDeleteFoo
  ```

[markup]: https://www.haskell.org/haddock/doc/html/ch03s08.html
[chunks]: https://www.haskell.org/haddock/doc/html/ch03s05.html
[tbaggery]: https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[progressive_disclosure]: https://en.wikipedia.org/wiki/Progressive_disclosure
