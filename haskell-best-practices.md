# Haskell practices

## Error throwing / exceptions

* Try very hard to avoid partial functions
* If you know a function is safe, but somehow weren't able to "prove" it to GHC,
  use your error string to state why you think the error is impossible. e.g.

  ```haskell
  halfOfEvens :: Rational -> [Rational] -> [Rational]
  halfOfEvens n = map (\d -> n `divEx` d) . filter (/= 0)
    where
      x `divEx` 0 = error
        "This should be impossible: We already filtered the list to remove zeroes"
      x `divEx` d = x / d
  ```

### Avoid `MonadFail`

Don't write functions constrained by `MonadFail`.

**Bad**:

```hs
data Email = Email

parseEmail :: MonadFail m => Text -> m Email
parseEmail txt
  | isEmail txt = return $ Email txt
  | otherwise = fail "that isn't an email!"

instance FromJSON Email where
  parseJSON = withText "Email" parseEmail
```

**Good**: use `Either String` concretely

```hs
data Email = Email

parseEmail :: Text -> Either String Email
parseEmail txt
  | isEmail txt = Right $ Email txt
  | otherwise = Left "that isn't an email!"

instance FromJSON Email where
  parseJSON = withText "Email" $ either fail pure . parseEmail
```

You can recover other side-effects easily:

```hs
hush . parseEmail :: Text -> Maybe Email

either throwString pure . parseEmail :: Text -> IO Email
```

**Justification**:

`MonadFail` over `Either` is a common foot-gun. We might expect `fail` to return
`Either String a`, but instead it throws an error. There are many other
surprising and problematic instances of `MonadFail` that make this abstraction
dangerous. Writing functions that abstract over `MonadFail` opens us up to these
surprising and hard to reason about errors.

### Learning resources

* [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
* [Haskell For Web Developers](http://www.stephendiehl.com/posts/haskell_web.html)

## Existentials

Why / when you want to use them:

* you have multiple types which satisfy some interface (type class)

AND

* you want to be able to add types to the interface without changing some
  supertype (i.e. 'open' rather than 'closed')

AND

* you want to delay the choice of which function from the interface to apply to
  the data (e.g. rather than using existentials for an interface with a single
  function, just apply that function, see
  [haskell-antipattern-existential-typeclass][])

  [haskell-antipattern-existential-typeclass]: https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/

AND

* you want to work with multiple types adhering to this interface without regard
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

* [Lens](https://github.com/ekmett/lens)
* Aeson lens:
  * [Haddocks](https://hackage.haskell.org/package/lens-aeson-1.0.0.3/docs/Data-Aeson-Lens.html)
  * [Blog post](http://dev.stephendiehl.com/hask/#lens-aeson)

## Script Haskell

[Stack Script Interpreter](https://github.com/commercialhaskell/stack/wiki/Script-interpreter)

## Data declaration with type family

Sometimes you have two types which are very similar, both in their
representations and in how they're used. For example,

```haskell
data Foo
  = Foo
  { a :: Int
  , b :: Int
  , c :: Int
  }

 data Bar
   = Bar
   { a :: Int
   , b :: Int
   , c :: Int
   , d :: Int
   }
```

It would be a pain to duplicate functions which operate on these types. A first
attempt to avoid that might be:

```haskell
data FooBar
  = FooBar
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

data FooBar f
  = FooBar
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

data FooBar (x :: FooBarType)
  = FooBar
   { a :: Int
   , b :: Int
   , c :: Int
   , d :: OnlyIfBar x Int
   }
```

With this approach `d :: FooBar Foo -> ()` (total absence of `d` would be the
best option, but without extensible records, `()` is the best we can do) and `d
:: FooBar Bar -> Int`.

## `newtype`s

Newtypes in Haskell are used for 3 primary purposes:

1. To document a type's meaning
1. To refine the type (i.e. limit it's inhabitants)
1. To define new/different instances

At FR, we seldom do 1. Instead we prefer to use `Tagged` and `TypeLits`:

```haskell
sf :: Tagged "City" String
sf = "San Francisco"
```

instead of

```haskell
newtype City = City { unCity :: String }
sf :: City
sf = City "San Francisco"
```

2 looks like:

```haskell
module Natural (Natural(), mkNatural) where

newtype Natural = Natural { unNatural :: Int }
mkNatural :: Int -> Maybe Natural
mkNatural n = if n >= 0 then Just (Natural n) else Nothing
```

3 looks like:

```haskell
newtype Add = Add { unAdd :: Int }
instance Monoid Add where
  mempty = 0
  mappend = (+)
newtype Mult = Mult { unMult :: Int }
instance Monoid Mult where
  mempty = 1
  mappend = (*)
```

## Phantom types

The `Tagged` example above makes use of a phantom type variable. The definition
of `Tagged` is:

```haskell
newtype Tagged t a = Tagged { untag :: a }
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
  | I Int
  | B Bool
  | Add Expr Expr
  | LessThan Expr Expr
  | Cond Expr Expr Expr
    deriving (Eq, Show)
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
eval (I i) = return $ VI i
eval (B b) = return $ VB b
eval (Add x y) = do
  VI x' <- eval x
  VI y' <- eval y
  return $ VI $ x' + y'
eval (LessThan x y) = do
  VI x' <- eval x
  VI y' <- eval y
  return $ VB $ x' < y'
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
perfectly legal (though of dubious utility): ```haskell data Thing a where
ThingA :: Int -> Thing Bool ThingB :: Bool -> Thing Int

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

* `IsList` has an associated type `Item` for representing the element of the
  list
* `MonoFoldable` uses an open type family `Elem` for representing the element of
  `mono`

Unfortunately, type families do not have great type inference. Furthermore,
`mono-traversable` exports a number of replacements for common `Prelude`
functions which rely on these type families.

The problem gets deeper when we realize that `mono-traversable` is a nearly
useless abstraction for our use-case. There are exactly *ZERO* uses of `omap` in
our codebase. We love `map`, `foldMap`, `traverse`, `for`, etc. These are
infinitely useful, but we are never for example traversing over the characters
of a `Text` type, which is `mono-traversable`'s primary inspiration. If our use
case demanded a lot of string munging, then maybe it would be useful. That just
isn't the case.

The fact is that `mono-traversable` is not an abstraction that targets our
use-case, it decreases type inference, increases cryptic type errors, and
increases boilerplate.

## Strictness

Add `LANGUAGE StrictData` to modules that contain plain records that will
undergo de/serialization (JSON, CSV, Database, etc).

## Appendix

### resources

* [profiling-cabal-projects](http://nikita-volkov.github.io/profiling-cabal-projects/)
* [Don Stewart on Core](http://stackoverflow.com/a/6121495)
* [Simon Peyton Jones on Core](https://youtu.be/uR_VzYxvbxg)
