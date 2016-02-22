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
