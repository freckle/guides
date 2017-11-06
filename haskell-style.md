# FrontRow Style Guide

Principles:

* **Maximize readability**
  * Code should be immediately readable by someone not familiar with that area. If you have to continuously refer to documentation, comments or dig into implementation to understand what's happening, the code isn't sufficiently self-descriptive and needs to be improved. When not possible, you must comment. Coding is a team sport, _always write with other developers in mind_
* **Be aware of the context**
* **Use your judgement**

There are some hard rules, but good style is context-sensitive.
Something that is good in preferable in one place could be bad in another.

# Haskell

* Max 120 col line length
* Comment following Haddock style

## Monad sequences and Haskell arrows

Monadic sequences should normally go in one direction, including `<-` from do notation

bad

    result <- m >>= return

good

    result <- return =<< m

That isn't to say we prever `=<<` over `>>=`

good

    result <-
          action4
      =<< action3
      =<< action2
      =<< action1

better

    result <-
          action1
      >>= action2
      >>= action3
      >>= action4

good. This is easy to read left-to-right and scales as the lambda grows

    m >>= (\x -> )

In general left bind does not scale well for inline code.
This is ok.

    (\x -> f x) =<< m

This is bad.

    (\x -> do f x
              g x
    ) =<< m

The lambda should be turned into a bound function.


## Distinguishing function arguments

Be careful of creating functions that have the same input type

    f :: Int -> Int -> Int -> Int

This is a good time to look at using a record to name the arguments or to use newtypes around the `Int`s. Note that the fact that the output is the same type as the input is not a concern: the below is fine

    f :: Int -> Int


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

Something small can go on one line. The scalable way of declaring things that maintains vertical alignment properties is

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

You should never need to align to preceding text. That is, your aligment point should always be 3rd, 5th, 7th etc. column (because we use two spaces of indenting).

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
which will need cause a noisy diff if we rename `GameSession` (each following line also needs to be indented)
```haskell
instance FromJSON (GameSession Never MathStandardAssignmentId) where
  parseJSON = withObject "cannot parse GameSession" $ \o ->
    GameSession2 <$> o .: "answers"
                 <*> o .: "domain-id"
                 <*> o .: "current-standard"
                 <*> o .: "sub-standard-perc"
                 <*> o .: "sub-sub-standard-perc"
                 <*> o .: "coins-gained"
                 <*> pure Never
```
do
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
  } deriving (Eq, Show)

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
-- Bad - do is indented 2 spaces, so the expressions following it have to be indented 5 spaces
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
-- Bad - aligning to case pushes expressions way to the left, and aligning arrows is fiddly
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
-- having to change anythign if/when they grow more lines
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
  - `base`
  - `mtl`
  - custom preludes
* Explicit imports for bringing lesser known functions in to scope
* Exclusionary imports for avoiding minor name clashes
  - `lens`
* Qualified imports for major name clashes
  - `containers`
  - `unordered-containers`
* Aliased imports for packaging and exporting many modules in a single module.
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
-- Overly open imports lead to increased ambiguity forcing common functions to be qualified.
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

### Importing types and qualifying
It is also common to explicitly import types from a module and also import it qualified.
```
import Data.Map (Map)
import qualified Data.Map as Map
```

### Abbreviating qualifications
There are a number of common abbreviations that are used in the community to qualify imports.
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

Imports are sorted, but with `qualified` versions appearing where they would
without the `qualified` word. If the same module appears un-`qualified` and
`qualified`, the `qualified` version comes second.


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

- Declare extensions in the module(s) that use them, i.e. don't use
  `default-extensions`

  This ensures a) the source code works un-changed with other Haskell tooling
  (e.g. `ghci`, `doctest`, etc) without further configuration and b) active
  extensions are visible in the modules they impact.

- Place extensions on their own line, and sort them

  ```haskell
  -- Bad
  {-# LANGUAGE OverloadedStrings, RecordWildCards,
      DataKinds #-}

  -- Good
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE RecordWildCards #-}
  ```
