# Haskell practices

## Error throwing / exceptions

TODO

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

## Appendix

#### resources

* http://www.google.com/url?q=http%3A%2F%2Fnikita-volkov.github.io%2Fprofiling-cabal-projects%2F&sa=D&sntz=1&usg=AFQjCNG5ilnpZzp9WSpBklXlJJw-5jF6Cg
