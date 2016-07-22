# Haskell practices

## Error throwing / exceptions

 * Try very hard to avoid partial functions
 * If you know a function is safe, but somehow weren't able to "prove" it to GHC, use your error string to state why you think the error is impossible. e.g.
 
 ```haskell
 halfOfEvens :: Rational -> [Rational] -> [Rational]
 halfOfEvens n = map (\d -> n `divEx` d) . filter (/= 0)
   where
     x `divEx` 0 = error "This should be impossible: We already filtered the list to remove zeroes"
     x `divEx` d = x / d
 ```

### Learning resources

* [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
* [Haskell For Web Developers](http://www.stephendiehl.com/posts/haskell_web.html)

## Existentials

Why / when you want to use them:

* you have multiple types which satisfy some interface (type class)
AND
* you want to be able to add types to the interface without changing some supertype (i.e. 'open' rather than 'closed')
AND
* you want to delay the choice of which function from the interface to apply to the data (e.g. rather than using existentials for an interface with a single function, just apply that function, https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/)
AND
* you want to work with multiple types adhering to this interface without regard for their original type

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

Lens is a very convenient way to read and update deeply nested structures. Works nicely both against raw JSON text and against Value types.

* https://github.com/ekmett/lens

* Aeson lens:
  * https://hackage.haskell.org/package/lens-aeson-1.0.0.3/docs/Data-Aeson-Lens.html
  * http://dev.stephendiehl.com/hask/#lens-aeson

## Script Haskell

https://github.com/commercialhaskell/stack/wiki/Script-interpreter

## Data declaration with type family

Sometimes you have two types which are very similar, both in their representations and in how they're used. For example,

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

It would be a pain to duplicate functions which operate on these types. A first attempt to avoid that might be:

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

This lets us share the commonalities and still differentiate in a safe way. But it's a little annoying that `Foo`'s `d` is wrapped in an `Identity` constructor at the value level. We can fix this final infelicity with type families:

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
With this approach `d :: FooBar Foo -> ()` (total absence of `d` would be the best option, but without extensible records, `()` is the best we can do) and `d :: FooBar Bar -> Int`.

## `newtype`s

Newtypes in Haskell are used for 3 primary purposes:
 1. To document a type's meaning
 2. To refine the type (i.e. limit it's inhabitants)
 3. To define new/different instances
 
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


## Appendix

#### resources

* http://nikita-volkov.github.io/profiling-cabal-projects/
