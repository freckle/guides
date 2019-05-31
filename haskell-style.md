# Haskell Style

Principles:

* **Maximize readability**
  * Code should be immediately readable by someone not familiar with that area.
    If you have to continuously refer to documentation, comments or dig into
    implementation to understand what's happening, the code isn't sufficiently
    self-descriptive and needs to be improved. When not possible, you must
    comment. Coding is a team sport, _always write with other developers in
    mind_
* **Be aware of the context**
* **Use your judgement**

There are some hard rules, but good style is context-sensitive. Something that
is good in preferable in one place could be bad in another.

## Line Length

Aim for 80 columns max. Long identifiers can sometimes make this tricky, so this
is not a hard-and-fast rule. `brittany` should handle this formatting for us in
most cases.

## Comments

Comments should follow [Haddock style](https://github.com/frontrowed/guides/blob/master/haskell-style.md#haddocks).

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

In general left bind does not scale well for inline code.
This is ok.

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

* comma-leading e.g.

```haskell
data Student
  = Student
  { firstName :: Text
  , lastName :: Text
  }
```

### Sum Types

Something small can go on one line. The scalable way of declaring things that
maintains vertical alignment properties is

``` haskell
data TransferTo
  = TransferToTeacher (Entity Teacher)
  | TransferToEmail   Email
```

## Function Type Signatures

Signatures should lead with arrows

``` haskell
:: Esqueleto m expr backend
=> TeacherId
-> expr (Entity Student)
-> m ()
```

## Alignment

You should never need to align to preceding text. That is, your alignment point
should always be 3rd, 5th, 7th etc. column (because we use two spaces of
indenting).

e.g. Instead of

```haskell
instance FromJSON (GameSession Never MathStandardAssignmentId) where
  parseJSON = withObject "cannot parse GameSession" $ \o ->
    GameSession <$> o .: "answers"
                <*> o .: "domain-id"
                <*> o .: "current-standard"
                <*> o .: "sub-standard-perc"
                <*> o .: "sub-sub-standard-perc"
                <*> o .: "coins-gained"
                <*> pure Never
```

which will cause a noisy diff if we rename `GameSession` (each following line
also needs to be indented):

```diff
 instance FromJSON (GameSession Never MathStandardAssignmentId) where
   parseJSON = withObject "cannot parse GameSession" $ \o ->
-   GameSession <$> o .: "answers"
-               <*> o .: "domain-id"
-               <*> o .: "current-standard"
-               <*> o .: "sub-standard-perc"
-               <*> o .: "sub-sub-standard-perc"
-               <*> o .: "coins-gained"
-               <*> pure Never
+   GameSession2 <$> o .: "answers"
+                <*> o .: "domain-id"
+                <*> o .: "current-standard"
+                <*> o .: "sub-standard-perc"
+                <*> o .: "sub-sub-standard-perc"
+                <*> o .: "coins-gained"
+                <*> pure Never
```

Instead, do

```haskell
instance FromJSON (GameSession Never MathStandardAssignmentId) where
  parseJSON =
    withObject "cannot parse GameSession" $ \o ->
      GameSession
        <$> o .: "answers"
        <*> o .: "domain-id"
        <*> o .: "current-standard"
        <*> o .: "sub-standard-perc"
        <*> o .: "sub-sub-standard-perc"
        <*> o .: "coins-gained"
        <*> pure Never
```

```diff
 instance FromJSON (GameSession Never MathStandardAssignmentId) where
   parseJSON =
     withObject "cannot parse GameSession" $ \o ->
-      GameSession
+      GameSession2
         <$> o .: "answers"
         <*> o .: "domain-id"
         <*> o .: "current-standard"
         <*> o .: "sub-standard-perc"
         <*> o .: "sub-sub-standard-perc"
         <*> o .: "coins-gained"
         <*> pure Never
```

### Operator-first

The above is an example of operator-first style. Which we use generally, as in
the following examples:

```hs
Foo
  <$> o .: "this"
  <*> o .: "that"

foo
  <&> foo .~ bar
  <.> baz .~ bat
```

```hs
fooBlahBlahHahaBlahBlahHaLongName
  . barBlahBlahHahaBlahLongName
  . bazBlahBlahHahaBlahHaLongName
  $ bat quix

Nothing -> left
  $ Text.pack "could not find parser for node \""
  <> name
  <> Text.pack "\" of type \""
  <> typ
  <> Text.pack "\" at "
  <> file
  <> Text.pack (": " ++ show n ++ ".")

logForwarderLambda :: Text -> Resource
logForwarderLambda envName = resource "LogForwarderLambda"
  $ LambdaFunctionProperties
  $ lambdaFunction
    ( lambdaFunctionCode
    & lfcS3Bucket ?~ "frontrow-ops"
    & lfcS3Key ?~ "logdna-lambda.zip"
    )
    "logdna_cloudwatch.lambda_handler"
    (GetAtt "LambdaRole" "Arn")
    (Literal Python27)
  & lfFunctionName ?~ Literal (envName <> "-log-dna-forwarder")
  & lfEnvironment ?~
    ( lambdaFunctionEnvironment
    & lfeVariables ?~
      [ ("LOGDNA_KEY", toJSON (Ref "LogDNAIngestionKey" :: Val Text))
      ]
    )
```

This is consistent with comma-first style for structural expressions, and is
generally easier to read in long functional expressions.

### More examples

#### Data declarations

```haskell
--- Bad
data SomeRecord = SomeRecord { someField :: Int
                             , someOtherField :: Double
                             } deriving (Eq, Show)

-- Good
data SomeRecord'
  = SomeRecord'
  { someField :: Int
  , someOtherField :: Double
  }
  deriving (Eq, Show)

-- Bad
data SomeSum = FirstConstructor
             | SecondConstructor Int
             | ThirdConstructor Double Text
             deriving (Eq, Show)

-- Good
data SomeSum'
  = FirstConstructor'
  | SecondConstructor' Int
  | ThirdConstructor' Double Text
  deriving (Eq, Show)
```

#### Do statements

```haskell
-- Bad - do is indented 2 spaces, so the expressions following it have to be
-- indented 5 spaces
someBinding =
  do x <- getLine
     y <- getLine
     putStrLn $ x <> y

-- Good - do is left hanging so the bindings are just indented 2 spaces
someBinding' = do
  x <- getLine
  y <- getLine
  putStrLn $ x <> y
```

#### Case expressions

```haskell
-- Bad - aligning to case pushes expressions way to the left, and aligning
-- arrows is fiddly
someBinding mx = case mx of
                   Nothing -> 0
                   Just x  -> x

-- Good - case on its own line, arrows don't need to be lined up
someBinding' mx =
  case mx of
    Nothing -> 0
    Just x -> x

-- If your case-alternatives are more complex, you can put them on their own line:
someBinding'' mx =
  case mx of
    Nothing ->
      putStrLn "Got nothin'"
    Just x ->
      putStrLn $ "Got " <> show x
```

#### Let expressions

```haskell
-- Bad - aligning multiple let bindings like this is fiddly
someBinding mx =
  let ma = fmap (+1) mx
      mb = fmap (*2) mx
  in (+) <$> ma <*> mb

-- Good - put `let` and `in` on its own line and then we can use normal spacing
someBinding''' mx =
  let
    ma = fmap (+1) mx
    mb = fmap (*2) mx
  in
    (+) <$> ma <*> mb

-- Fine to keep on same line when you only have one binding, but use your judgement
someBinding'' mx =
  let ma = fmap (+1) mx
  in maybe 0 (*2) ma
```

#### Where bindings

Expressions with `where`-bindings should place the body of the expression (if on
the next line) and the `where`-bindings at the same (2-space) indentation, with
the `where` keyword de-dented 1 space. Note that this causes an odd, 1-space
indentation of the `where` keyword.

```haskell
-- Bad - the `where` disappears
someBinding mx = f <$> mx <*> mx where f x y = x * x + y * y

-- Bad - the alignment is fiddly
someBinding' mx = g $ f <$> mx <*> mx
  where f x y = x * x + y * y
        g = maybe 0 (*2)

-- OK, but dissallowed in the interest of consistency
someBinding' mx = g $ f <$> mx <*> mx
  where
    f x y = x * x + y * y
    g = maybe 0 (*2)

-- Good
someBinding mx =
  f <$> mx <*> mx
 where
  f x y = x * x + y * y

-- Good - use the same indentation for non-multi-line expressions to avoid
-- having to change anything if/when they grow more lines
someBinding mx = f <$> mx <*> mx
 where
  f x y = x * x + y * y
```

#### Lists and Tuples

```haskell
-- Short lists and tuples can be placed on one line
names = ["Joe", "Bob", "Sam"]
car = ("Acura", "Integra", 2000)

-- Multiline lists and tuples and have commas first
names' =
  [ "Joe"
  , "Bob"
  , "Same"
  ]

car' =
  ( "Acura"
  , "Integra"
  , 2000
  )

-- You're more likely to see multiline records than tuples
teacher =
  Teacher
    { firstName = "First"
    , lastName = "Last"
    , schoolId = 123
    , hasPremium = True
    }
```

## Imports

Haskell's modules expose some variety in import style:

* Open imports
* Explicit imports
* Exclusionary imports
* Qualified imports
* Aliased imports

Good style prefers:

* Open imports for common libraries
  * `base`
  * `mtl`
  * custom preludes
* Explicit imports for bringing lesser known functions in to scope
* Exclusionary imports for avoiding minor name clashes
  * `lens`
* Qualified imports for major name clashes
  * `containers`
  * `unordered-containers`
* Qualified imports for "ad-hoc module schema" (see example)
* Aliased imports for packaging and exporting many modules in a single module.
  * creating a custom prelude

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

**NOTE**: if explicit imports exceed 80 columns, switch to a (sorted) list:

```haskell
-- Bad
-- Long lines are hard to scan, inserting or removing an item is a noisier diff,
-- and it's difficult to sort a horizontal list
import System.IO (hPutStrLn, stderr, stdout, withFile, IOMode(..), hGetContents, hFlush, hClose)

-- Good
import System.IO
  ( IOMode(..)
  , hClose
  , hFlush
  , hGetContents
  , hPutStrLn
  , stderr
  , stdout
  , withFile
  )
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

It is also common to explicitly import types from a module and also import it
qualified.

```
import Data.Map (Map)
import qualified Data.Map as Map
```

### Abbreviating qualifications

There are a number of common abbreviations that are used in the community to
qualify imports.

```haskell
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

import qualified Data.List.NonEmpty as NE

import qualified Data.Sequence as Seq
```

### Import groups

Put one blank line between `module`-`where` and the start of your `import`s. Put
your preferred prelude (when explicit) first, followed by a blank line, then the
rest of your imports.

The main `import` group should be maintained by our `stylish-haskell`
configuration.

For vim users,

```vim
:stylish-haskell %

" or visually select the imports and
:'<,'>!stylish-haskell
```

An example of its results at the time of this writing is shown below, but what
it actually does is less important than the fact that it's automated.

```haskell
-- Bad
-- Improper spacing, improper sorting
module Foo
  ( bar
  , baz
  ) where
import qualified Data.Map as Map
import TextAssets.S3
import Data.Text (Text)
import Unit
import Json
import Control.Lens
import qualified Data.Set as Set
import ClassyPrelude
import Network.AWS.S3
import Data.Set (Set)
import qualified Data.Text as T
import Network.AWS
import Data.Map (Map)
import Data.Conduit

-- Good
module Foo
  ( bar
  , baz
  ) where

import ClassyPrelude

import Control.Lens
import Data.Conduit
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Json (Json)
import qualified Json as J
import Network.AWS
import Network.AWS.S3
import TextAssets.S3
import Unit
```

## Exports

Use sorted, multi-line exports. There are two exceptions to this rule:

1. A single-line export for `Main`, when it only exports `main`:

   ```haskell
   module Main (main) where
   ```

1. If the order of exports matters in your desired Haddock output, you may
   violate sorting to achieve it.

Otherwise:

```haskell
-- Bad
module Driver (scienceOptions, socialStudiesOptions, mainWith) where

-- Good
module Driver
  ( mainWith
  , scienceOptions
  , socialStudiesOptions
  ) where
```

## Declaring Extensions

* All Haskell packages MUST use the following `default-extensions`:

  ```yaml
  default-extensions:
    - BangPatterns
    - DeriveAnyClass
    - DeriveFoldable
    - DeriveFunctor
    - DeriveGeneric
    - DeriveLift
    - DeriveTraversable
    - DerivingStrategies
    - FlexibleContexts
    - FlexibleInstances
    - GADTs
    - GeneralizedNewtypeDeriving
    - LambdaCase
    - MultiParamTypeClasses
    - NoImplicitPrelude
    - NoMonomorphismRestriction
    - OverloadedStrings
    - RankNTypes
    - RecordWildCards
    - ScopedTypeVariables
    - StandaloneDeriving
    - TypeApplications
    - TypeFamilies
  ```

  **NOTE**: `NoImplicitPrelude` may be omitted in packages using the normal,
  implicit `Prelude` everywhere.

  This defines a consistent, and minimally-extended Haskell environment. Other
  extensions MUST be defined via LANGUAGE pragmas in the modules where they're
  needed.

* Place extensions on their own line, and sort them

  ```haskell
  -- Bad
  {-# LANGUAGE OverloadedStrings, RecordWildCards,
      DataKinds #-}

  -- Good
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE RecordWildCards #-}
  ```

* Leave a blank line after the extensions list

  ```haskell
  {-# LANGAUGE OverloadedStrings #-}

  module Foo
    ( foo
    ) where

  ```

  ```haskell
  {-# LANGAUGE OverloadedStrings #-}

  -- |
  --
  -- The Foo module does the foo-ing
  --
  module Foo
    ( foo
    ) where

  ```

## Haddocks

Haskell modules should be commented with valid Haddock documentation. We are not
yet requiring a certain level of coverage, but it is strongly encouraged.

### General guides

* Use proper Haddock [markup][]
* Link all identifiers, anywhere they appear

  Bad

  ```hs
  -- | Construct a @'FlipFlop'@
  --
  -- If the size is right, you will get a @Right FlipFlop@, otherwise a @Left@
  --
  ```

  Good

  ```hs
  -- | Construct a @'FlipFlop'@
  --
  -- If the size is right, you will get a @'Right' 'FlipFlop'@, otherwise a
  -- @'Left'@.
  --
  ```

* Use leading documentation (`-- |`) for top-level definitions and trailing
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

When a Body is _not_ present (see below), no newline is required between a Summary and its associated top-level definition:

Bad

```hs
-- | The worse of the @'Thing'@ twins
-- 
badThing :: Thing
badThing = Thing 1
```

Good

```hs
-- | The better of the @'Thing'@ twins
goodThing :: Thing
goodThing = Thing 2
```

### Body

Bodies are optional but encouraged. When present, the following applies:

* Wrap non-literal content at 80 columns (not our usual 120)
* Surround block elements by a line of whitespace

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

* Lists receive a hanging indent

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

* Organize your exports by logical groups or [progressive
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

* Consider adding section documentation

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

* If you want to separate the definitions *in* the module, use [named
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
    ) where

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
