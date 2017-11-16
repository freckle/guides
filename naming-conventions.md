# Naming Conventions

## Postgres

Postgres uses `snake_case`. Tables should be plural (e.g. `teachers`, `students`). Text-based enums are currently also `snake_case` (except domain abbreviations and math system which are `ALLCAPS`), though this makes interoperating with Haskell and JavaScript non-obvious.

## Haskell

Haskell uses `camelCase` for identifiers and `TitleCase` for types and constructors. The latter is enforced by the language. Names that are being ignored should either use the wildcard `_` or specify what is being ignored (e.g. `_teacher`, `_standard`).

### Polymorphic Types

Prefer short, single-letter names for polymorphic type variables and arguments. You'll notice a number of conventions with type variables.

Unconstrained type variables often start with `a` and continue alphabetically
```haskell
id :: a -> a
const :: a -> b -> a
```

Polymorphic lists are often "pluralized" by appending `s`
```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter predicate (a:as)
  | predicate a = a : bs
  | otherwise = bs
 where
  bs = filter predicate as
```

`f` is for `Functor` and sometimes `Applicative` and `Alternative`
```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
guard :: Alternative f => Bool -> f ()
```

`t` might be `Traversable` or `Foldable`
```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
```

`m` is almost always `Monad` or something monad-transformer related
```haskell
when :: Monad m => Bool -> m () -> m ()
liftIO :: MonadIO m => IO a -> m a
```

`s` is for state and `e` is for environment
```haskell
put :: MonadState s m => s -> m ()
ask :: MonadReader e m => m e
```

These are conventions only, but picking something unusual for a common typeclass might surprise other readers.

### Concrete Types

Avoid using single-letter names or abbreviations for values with a concrete type, especially if that type is domain-specific. To crib from the [Swift API Design Guidelines](https://swift.org/documentation/api-design-guidelines/#fundamentals), "Clarity is more important than brevity."

For example, don't do this:
```haskell 
fetchCourseMemberships :: MonadIO m => Entity Teacher -> SqlReadT m [Entity CourseMembership]
fetchCourseMemberships (Entity tId teacher) =
  select $ from $ \(ts `InnerJoin` cs `InnerJoin` cms) -> do
    on $ cs ^. CourseId ==. cms  ^. CourseMembershipCourseId
    on $ cs ^. CourseTeacherId ==. ts ^. TeacherId
    where_ $
      case teacherSchoolId teacher of
        Just sId ->
          ts ^. TeacherSchoolId ==. val (Just sId)
        Nothing ->
          ts ^. TeacherId ==. val tId
    pure cms
```

Do this instead:
```haskell
fetchCourseMemberships :: MonadIO m => Entity Teacher -> SqlReadT m [Entity CourseMembership]
fetchCourseMemberships (Entity teacherId teacher) =
  select $ from $ \(teachers `InnerJoin` courses `InnerJoin` memberships) -> do
    on $ courses ^. CourseId ==. memberships  ^. CourseMembershipCourseId
    on $ courses ^. CourseTeacherId ==. teachers ^. TeacherId
    where_ $
      case teacherSchoolId teacher of
        Just schoolId ->
          teachers ^. TeacherSchoolId ==. val (Just schoolId)
        Nothing ->
          teachers ^. TeacherId ==. val teacherId
    pure memberships
```

We tolerate a few abbreviations as part of identifiers (e.g. the `num` portion of `numQuestionsAnswered`), but there doesn't seem to be any broad consensnus about this.

## JavaScript

JavaScript uses `camelCase` for identifiers and `TitleCase` for classes and React components. For flow types, we generally append a `T` to the end of the type, e.g. `SkillT`. Names that are being ignored should be prefixed with an underscore (e.g. `_teacher`, `_standard`). It's a common mistake (especially for Haskellers) to use a lone `_` and accidentally overwrite underscore in that scope.

## Problems

Serialization of string-enums to JSON and postgres is all over the place. We have examples of `snake_case`, `camelCase`, `TitleCase`, and `ALLCAPS`.

If we have DB-level enums that are not shared with the frontend, it makes sense to use the postgres convention of `snake_case`. However, if we're planning to pass enums to the frontend, then we should use whatever the JavaScript convention is. JavaScript is also using pretty much everything, so it's hard to say what we should be using. I (Parks) usually use `TitleCase` for JavaScript-only enums, but only because that mimics Haskell enums.

Haskell pretty much enforces a style at the language-level by requiring that the first character of each constructor be capitalized. We're using `TitleCase` everywhere except for `MathSystem` and `DomainAbbreviation` which are `ALLCAPS`.
