# Haskell practices

**NOTE**: the code examples here may not follow our current style. They have not
been maintained for that as things have evolved.

## Prelude

- Packages should use `NoImplicitPrelude`
- Packages can make a package-local prelude module, provided the other guides
  here are followed

- Packages should not have more than one local prelude

  ```hs
  -- Bad
  module Handler1 where

  import Freckle.Api.Prelude -- re-exports Persist stuff by default

  module Handler2 where

  import Freckle.Api.Prelude.Esqueleto -- hides Persist and re-exports Esqueleto

  -- Good
  module Handler1 where

  import Freckle.Api.Prelude -- re-exports neither Persist or Esqueleto

  import Freckle.Api.Persist -- brings in Persist

  module Handler2 where

  import Freckle.Api.Prelude

  import Freckle.Api.Esqueleto -- brings in Esqueleto
  ```

- Local preludes should be named `{PackageNamespace}.Prelude`

  ```hs
  -- Bad
  import Import

  import Freckle.Import

  import Freckle.Entities.Import

  -- Good
  import Prelude -- Library without custom prelude module

  import Freckle.App.Prelude -- App without custom prelude module

  import Freckle.Jobs.Prelude -- App with custom prelude module

  import Yesod.Auth.OAuth2.Prelude -- Library with custom prelude module
  ```

- Local preludes that re-export `Prelude` should hide unsafe functions

- Libraries must use `Prelude`. Do not force an alternative prelude choice on
  end-users.

- Applications should use `Freckle.App.Prelude`. If not, they must use
  `Prelude`.

  If we decide in the future to use a 3rd-party alternative prelude, we will do
  that within `Freckle.App.Prelude`.

- When specifically importing an "unsafe" function from `Prelude`, import it
  restricted and `qualified as Unsafe` to make that clear:

  ```hs
  -- Bad
  import Prelude (last)

  getDockerImageTag :: DockerImage -> Text
  getDockerImageTag =
    -- This is safe because we can only ever construct valid 'DockerImage'
    -- values that contain the @:@ character
    last . T.splitOn ":" . unDockerImage

  -- Good
  import qualified Prelude as Unsafe (last)

  getDockerImageTag :: DockerImage -> Text
  getDockerImageTag =
    -- This is safe because we can only ever construct valid 'DockerImage'
    -- values that contain the @:@ character
    Unsafe.last . T.splitOn ":" . unDockerImage
  ```

## Error throwing / exceptions

- Try very hard to avoid partial functions
- If you know a function is safe, but somehow weren't able to "prove" it to GHC,
  use your error string to state why you think the error is impossible. e.g.

  ```haskell
  halfOfEvens :: Rational -> [Rational] -> [Rational]
  halfOfEvens n = map (\d -> n `divEx` d) . filter (/= 0)
    where
      x `divEx` 0 = error
        "This should be impossible: We already filtered the list to remove zeroes"
      x `divEx` d = x / d
  ```

### Learning resources

- [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
- [Haskell For Web Developers](http://www.stephendiehl.com/posts/haskell_web.html)

## Do not prefix record fields

There was once a time when record field names needed to be unique, and so it was
conventional to prefix them with a constructor name:

```haskell
data StudentAssignmentReport =
  StudentAssignmentReport
    { studentAssignmentReportId :: UUID
    , studentAssignmentReportGrade :: Percentage
    , studentAssignmentReportTitle :: Text
    }
```

Thanks to `NoFieldSelectors`, `DuplicateRecordFields`, and `OverloadedRecordDot`,
it does not matter whether a record field has the same name as anything else, and
so the prefixed field style is no longer useful. Such names should be shortened:

```haskell
data StudentAssignmentReport =
  StudentAssignmentReport
    { id :: UUID
    , grade :: Percentage
    , title :: Text
    }
```

## Existentials

Why / when you want to use them:

- you have multiple types which satisfy some interface (type class)

AND

- you want to be able to add types to the interface without changing some
  supertype (i.e. 'open' rather than 'closed')

AND

- you want to delay the choice of which function from the interface to apply to
  the data (e.g. rather than using existentials for an interface with a single
  function, just apply that function, see
  [haskell-antipattern-existential-typeclass][])

  [haskell-antipattern-existential-typeclass]: https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/

AND

- you want to work with multiple types adhering to this interface without regard
  for their original type

Bad example:

```haskell
data ShowBox = forall s. Show s => SB s
heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]
```

Instead you can just show:

```haskell
heteroList :: [String]
heteroList = [show (), show 5, show True]
```

Okay example:

```haskell
data ShowEnum = forall s. (Show s, Enum s) => SE s

showNexts = map (show . succ)
showPrevs = map (show . pred)
```

## Lens

Lens is a very convenient way to read and update deeply nested structures. Works
nicely both against raw JSON text and against Value types.

- [Lens](https://github.com/ekmett/lens)
- Aeson lens:
  - [Haddocks](https://hackage.haskell.org/package/lens-aeson-1.0.0.3/docs/Data-Aeson-Lens.html)
  - [Blog post](http://dev.stephendiehl.com/hask/#lens-aeson)

Some Engineers prefer named functions (`view`, `set`) and some prefer the
equivalent operators (`(^.)`, `(.~)`). We do not prescribe one way or the other,
but do not mix them within a module.

```hs
-- Bad
& set fooL "foo"
. barL ?~ "bar"
. over bazL (<> 2)

-- Good
& set fooL "foo"
. set barL (Just "bar")
. over bazL (<> 2)

-- Also good
& fooL .~ "foo"
. barL ?~ "bar"
. bazL %~ (<> 2)

-- Also good
& fooL .~ "foo"
. barL ?~ "bar"
. bazL <>~ 2
```

## Script Haskell

[Stack Script Interpreter](https://github.com/commercialhaskell/stack/wiki/Script-interpreter)

## Data declaration with type family

Sometimes you have two types which are very similar, both in their
representations and in how they're used. For example,

```haskell
data Foo = Foo
  { a :: Int
  , b :: Int
  , c :: Int
  }

data Bar = Bar
  { a :: Int
  , b :: Int
  , c :: Int
  , d :: Int
  }
```

It would be a pain to duplicate functions which operate on these types. A first
attempt to avoid that might be:

```haskell
data FooBar = FooBar
  { a :: Int
  , b :: Int
  , c :: Int
  , d :: Maybe Int
  }
```

but we lose value type safety/documentation here.

A better approach is to parameterize the field which varies:

```haskell
type Never = Proxy
type Always = Identity

data FooBar f = FooBar
  { a :: Int
  , b :: Int
  , c :: Int
  , d :: f Int
  }

type Foo = FooBar Never
type Bar = FooBar Always
```

This lets us share the commonalities and still differentiate in a safe way. But
it's a little annoying that `Foo`'s `d` is wrapped in an `Identity` constructor
at the value level. We can fix this final infelicity with type families:

```haskell
data FooBarType = Foo | Bar

type family OnlyIfBar (x :: FooBarType) (a :: *) :: * where
  OnlyIfBar Foo a = ()
  OnlyIfBar Bar a = a

data FooBar (x :: FooBarType) = FooBar
  { a :: Int
  , b :: Int
  , c :: Int
  , d :: OnlyIfBar x Int
  }
```

With this approach `d :: FooBar Foo -> ()` (total absence of `d` would be the
best option, but without extensible records, `()` is the best we can do) and
`d :: FooBar Bar -> Int`.

## `newtype`s

Newtypes in Haskell are used for 3 primary purposes:

1. To document a type's meaning
1. To refine the type (i.e. limit it's inhabitants)
1. To define new/different instances

1 allows us to communicate clearly and increase type safety:

```haskell
newtype City = City
  { unCity :: String
  }

sf :: City
sf = City "San Francisco"
```

Some legacy code at FR uses `Tagged` and `TypeLits`. This style [should be
avoided](https://tech.freckle.com/2020/10/26/tagged-is-not-a-newtype/) in favor
of proper newtypes.

```haskell
-- Don't do this
sf :: Tagged "City" String
sf = "San Francisco"
```

2 looks like:

```haskell
module Natural (Natural(), mkNatural) where

newtype Natural = Natural
  { unNatural :: Int
  }

mkNatural :: Int -> Maybe Natural
mkNatural n = if n >= 0 then Just (Natural n) else Nothing
```

3 looks like:

```haskell
newtype Add = Add
  { unAdd :: Int
  }

instance Monoid Add where
  mempty = 0
  mappend = (+)

newtype Mult = Mult
  { unMult :: Int
  }

instance Monoid Mult where
  mempty = 1
  mappend = (*)
```

## Phantom types

The `Tagged` example above makes use of a phantom type variable. The definition
of `Tagged` is:

```haskell
newtype Tagged t a = Tagged
  { untag :: a
  }
```

Note how the type variable `t` does not appear on the right-hand-side of the
equals-sign. It's only used to add type-level information to a type - it's never
used at the value level. We call this kind of type variable a phantom type
variable, or occasionally, just a phantom type.

A more useful example is hashing passwords. We might want to represent passwords
as either raw text or hashed text and have operations that only work on one
kind:

```haskell
module Password
  ( Password
  , PasswordState
  , rawPassword
  , hashPassword
  , comparePassword
  )
where

data PasswordState = Raw | Hashed

newtype Password (s :: PasswordState) = Password Text

rawPassword :: Text -> Password 'Raw
rawPassword = Password

-- Note: this hash may be less secure than advertised
secureHash :: Text -> Text
secureHash t = reverse $ t ++ t

hashPassword :: Password 'Raw -> Password 'Hashed
hashPassword (Password r) = Password (secureHash r)

comparePassword :: Password 'Raw -> Password 'Hashed -> Bool
comparePassword (Password r) (Password h) = secureHash r == h
```

We're using a promoted datatype (`PasswordState`) to encode whether a `Password`
is plaintext or hashed. Note that we do not expose the constructor for
`Password`, and instead expose a smart constructor `rawPassword` and a hashing
function `hashPassword`. If we expose the constructor, a malicious (or
forgetful) user could construct a value of `Password 'Hashed` that contains a
plaintext password.

## GADTs

Closely related to phantom types are Generalized Algebraic Data Types or GADTs.
Suppose we had the following representation of a small programming language with
integers, bools, addition, and conditions:

```haskell
data Expr
  = I Int
  | B Bool
  | Add Expr Expr
  | LessThan Expr Expr
  | Cond Expr Expr Expr
  deriving stock (Eq, Show)
```

We can construct values like `Add (I 1) (I 3)` to represent `1 + 3`, but there's
nothing preventing us from constructing values like `Add (I 1) (B True)`.
Furthermore, we can only detect this kind of problem at runtime. Writing an
evaluation function for this data type is frustrating:

```haskell
data Value
  = VI Int
  | VB Bool

eval :: Expr -> Maybe Value
eval (I i) = pure $ VI i
eval (B b) = pure $ VB b
eval (Add x y) = do
  VI x' <- eval x
  VI y' <- eval y
  pure $ VI $ x' + y'
eval (LessThan x y) = do
  VI x' <- eval x
  VI y' <- eval y
  pure $ VB $ x' < y'
eval (Cond c t f) = do
  VB c' <- eval c
  if c'
    then eval t
    else eval f
```

We're relying on the `Monad` instance for `Maybe` to call `fail` on pattern
match failures when we pass something like `Add (I 1) (B True)` at runtime. We
also have to make an extra datatype to represent values.

You can try to do something smarter with phantom types, existential
quantification, and smart constructors:

```haskell
data Expr a
  = I Int
  | B Bool
  | Add (Expr a) (Expr a)
  | forall b. LessThan (Expr b) (Expr b)
  | forall c. Cond (Expr c) (Expr a) (Expr a)

int :: Int -> Expr Int
int = I

bool :: Bool -> Expr Bool
bool = B

add :: Expr Int -> Expr Int -> Expr Int
add = Add

lessThan :: Expr Int -> Expr Int -> Expr Bool
lessThan = LessThan

cond :: Expr Bool -> Expr a -> Expr a -> Expr a
cond = Cond

eval :: Expr a -> a
eval (I i) = i
eval (B b) = b
eval (Add x y) = eval x + eval y
eval (LessThan x y) = eval x < eval y
eval (Cond c t f)
  | eval c = eval t
  | otherwise = eval f
```

Unfortunately this doesn't compile. You'll get errors like:

```plaintext
Couldn't match expected type ‘a’ with actual type ‘Bool’
  ‘a’ is a rigid type variable bound by
      the type signature for eval :: Expr a -> a at x.hs:57:9
Relevant bindings include eval :: Expr a -> a (bound at x.hs:58:1)
In the expression: b
In an equation for ‘eval’: eval (B b) = b
```

Everything up to the `eval` function works, but then we don't have any evidence
when matching `B b` that `Expr a` should be `Expr Bool`. We're not carrying that
information around!

This is what GADTs are for - they let you carry around extra type evidence in
your constructors:

```haskell
data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  LessThan :: Expr Int -> Expr Int -> Expr Bool
  Cond :: Expr Bool -> Expr a -> Expr a -> Expr a

deriving instance Eq a => Eq (Expr a)
deriving instance Show a => Show (Expr a)

eval :: Expr a -> a
eval (I i) = i
eval (B b) = b
eval (Add x y) = eval x + eval y
eval (LessThan x y) = eval x < eval y
eval (Cond c t f)
  | eval c = eval t
  | otherwise = eval f
```

When we pattern match on `B` here, we gain access to evidence that `Expr a` is
`Expr Bool`, and so on for the other constructors. Furthermore, GHC won't even
let us construct a value like `Add (I 1) (B True)`:

```haskell
Couldn't match type ‘Bool’ with ‘Int’
 Expected type: Expr Int
   Actual type: Expr Bool
In the second argument of ‘Add’, namely ‘(B True)’
In the expression: Add (I 1) (B True)
```

If you're curious how this works, you can dump the Core (`-ddump-simpl`, see
Appendix) to see that each constructor is literally carrying around an extra
parameter as evidence that allows GHC to insert safe type casts from `a` to
`Int` or `Bool` (or whatever) inside a pattern match.

Note that the constructor's argument type doesn't have to match the type
argument of the data type (see `LessThan` and `Cond` above). The following is
perfectly legal (though of dubious utility):

```haskell
data Thing a where
ThingA :: Int -> Thing Bool
ThingB :: Bool -> Thing Int
```

```haskell
f :: Thing a -> a
f (ThingA a) = a == 0
f (ThingB b) = if b then 1 else 2

```

## Avoid `OverloadedLists` and `MonoTraversable`

`OverloadedLists` depend on a typeclass `IsList`:

```haskell
class IsList l where
  type Item l
  fromList  :: [Item l] -> l
  toList    :: l -> [Item l]
```

The `mono-traversable` package defines a series of typeclasses:

```haskell
type family Element mono

class MonoFoldable mono where
  ofoldMap :: Monoid m => (Element mono -> m) -> mono -> m
...
```

Each of these typeclasses rely on type families:

- `IsList` has an associated type `Item` for representing the element of the
  list
- `MonoFoldable` uses an open type family `Elem` for representing the element of
  `mono`

Unfortunately, type families do not have great type inference. Furthermore,
`mono-traversable` exports a number of replacements for common `Prelude`
functions which rely on these type families.

The problem gets deeper when we realize that `mono-traversable` is a nearly
useless abstraction for our use-case. There are exactly _ZERO_ uses of `omap` in
our codebase. We love `map`, `foldMap`, `traverse`, `for`, etc. These are
infinitely useful, but we are never for example traversing over the characters
of a `Text` type, which is `mono-traversable`'s primary inspiration. If our use
case demanded a lot of string munging, then maybe it would be useful. That just
isn't the case.

The fact is that `mono-traversable` is not an abstraction that targets our
use-case, it decreases type inference, increases cryptic type errors, and
increases boilerplate.

## Strictness

Add `LANGUAGE StrictData` to modules that contain only plain records that will
undergo de/serialization (JSON, CSV, Database, etc).

## Testing

### Export Lists

```hs
-- Bad
module FooSpec where

-- Good
module FooSpec
  ( spec
  ) where
```

### Imports

Each project should have a `TestImport` used for all specs. Optionally, a
`TestImport.IO` version can exist for writing non-App specs in `IO`.

```hs
-- Bad: using TestImport as a module and not the "prelude"
module Project.FooSpec
  ( spec
  ) where

import Project.Prelude

import Project.App
import Project.Lib
import TestImport

-- Also Bad: Using Hspec directly, and treating it like a "prelude"
module Project.FooSpec
  ( spec
  ) where

import Test.Hspec

import Project.Prelude
import Project.App
import Project.Lib

-- Good: exactly one prelude module, and it is TestImport
module Project.FooSpec
  ( spec
  ) where

import TestImport

import Project.App

spec :: Spec
spec = withApp loadApp $ do
  describe "myAppFunction" $ do

-- Also Good: same, but for an IO spec
module Project.FooSpec
  ( spec
  ) where

import TestImport.IO

import Project.Lib

spec :: Spec
spec = do
  describe "myPureFunction" $ do
```

#### CallStack

Hspec uses the `HasCallStack` to report the location of test failures. It's
important that any functions that make assertions (or call functions that make
assertions) have the proper constraint.

It's also important that the top-level `spec` **does not**. Otherwise, failures
are attributed to the generated `Spec.hs` file.

```hs
-- Bad
shouldApproximate :: Double -> Double -> Expectation

-- Good
shouldApproximate :: HasCallStack => Double -> Double -> Expectation

-- Best (see below)
shouldApproximate
  :: (HasCallStack, MonadIO m)
  => Double
  -> Double
  -> m ()
```

```hs
-- Bad
spec :: HasCallStack => Spec
spec = do

-- Good
spec :: Spec
spec = do
```

### Expectations vs Assertions

Prefer Expectations over Assertions

```hs
-- Bad
assertEqual "..." a b

-- Good
a `shouldBe` b
```

Except for functions that return values as part of their assertions, like
`assertJust`, or `assertRight`.

### Exceptions

Use `expectationFailure` when possible. We have an overloaded version that is `m a` instead of `m ()`, which makes it more useful.

```hs
-- Bad
case x of
  Left er -> throwString err
  Right x -> x `shouldBe` y

-- Good
case x of
  Left er -> expectationFailure err
  Right x -> x `shouldBe` y

-- Best (relies on overloaded expectationFailure)
x <- assertRight $ ...
x `shouldBe` y
```

Though not required, pattern-match failures are an acceptable way to fail a
test, since the example type (`AppExample`, `YesodExample`) has usually defined
`MonadFail(fail)` as `expectationFailure`.

For example, a test about a user in the DB can incorporate that assertion,

```hs
-- Meh
user <- assertJust =<< getDB userId
userFoo user `shouldBe` Foo

-- Nice
Just user <- getDB userId
userFoo user `shouldBe` Foo
```

#### Example Descriptions

Somehow the "should" Railsism took hold in our code-base. There's no need for it
and it can often make spec lines so long that they indent strangely.

Also, avoid the word "successfully". When expecting something to happen, that
you expect it to happen "successfully" can be assumed.

```hs
-- Bad
it "should successfully deduplicate the users"
  $ withGraph
  $ do
    omgI'mAllTheWayOverHereNow

-- OK
it "successfully deduplicates the users" $ ...

-- Best
it "deduplicates the users" $ withGraph $ do
  lookHowNiceIFitNow
```

#### Lifted Expectations

By default, we use and write expectations that are in `MonadIO m`. If you are
using `TestImport` or `TestImport.IO` you should not have any ambiguity issues.

The following are some things that may cause them, along with their fixes:

- Using `TestImport`, but not `withApp`

  Use `TestImport.IO`.

- Mixing `withApp` and non-`withApp` stanzas

  Put everything under `withApp`. (Except `prop`, see below.)

- Using `it "..." $ property`

  Use `prop "..."`.

- Using `prop` under `withApp`

  Pull it out.

- Using `shouldX` with `prop`

  Use a pure `Bool` expression with `prop`.

If you do run into ambiguity, prefer [Type Applications][] (over type signatures
or annotations) to resolve it.

[type applications]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html

## Appendix

### resources

- [profiling-cabal-projects](http://nikita-volkov.github.io/profiling-cabal-projects/)
- [Don Stewart on Core](http://stackoverflow.com/a/6121495)
- [Simon Peyton Jones on Core](https://youtu.be/uR_VzYxvbxg)
