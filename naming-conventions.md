# Naming Conventions

## Postgres

Postgres uses `snake_case`. Tables should be plural (e.g. `teachers`,
`students`). Text-based enums are currently also `snake_case` (except domain
abbreviations and math system which are `ALLCAPS`), though this makes
interoperating with Haskell and JavaScript non-obvious.

### Foreign Keys

If a table has a foreign key to another table, remove the _product_ and
_subject_ prefix from the foreign key column's name unless doing so would
introduce ambiguity.

Don't do this:

```sql
CREATE TABLE ela_adaptive_skill_practice_paragraphs (
  id integer PRIMARY KEY NOT NULL,
  ela_adaptive_skill_practice_content_id uuid REFERENCES ela_adaptive_skill_practice_content NOT NULL,
  type text NOT NULL,
  content text NOT NULL,
  caption text,
  position integer NOT NULL,
  UNIQUE (ela_adaptive_skill_practice_content_id, position),
  CONSTRAINT valid_ela_adaptive_skill_practice_paragraph_position CHECK (position >= 0)
);
```

Do this:

```diff
CREATE TABLE ela_adaptive_skill_practice_paragraphs (
  id integer PRIMARY KEY NOT NULL,
- ela_adaptive_skill_practice_content_id uuid REFERENCES ela_adaptive_skill_practice_content NOT NULL,
+ content_id uuid REFERENCES ela_adaptive_skill_practice_content NOT NULL,
  type text NOT NULL,
  content text NOT NULL,
  caption text,
  position integer NOT NULL,
  UNIQUE (ela_adaptive_skill_practice_content_id, position),
  CONSTRAINT valid_ela_adaptive_skill_practice_paragraph_position CHECK (position >= 0)
);
```

#### Why?

We only need to convey that this field is a pointer to "content". Any other
information we could encode can be understood from context. Furthermore, the
elided information is more likely to change. In fact, we no longer refer to
this product as "AdaptiveSkillPractice". It's just "SkillsPractice" now.
However, because we've encoded superfluous information in the column, the
Haskell and JSON representations do too, which makes changing it a multi-step
process.

## Haskell

Haskell uses `camelCase` for identifiers and `TitleCase` for types and
constructors. The latter is enforced by the language. Names that are being
ignored should either use the wildcard `_` or specify what is being ignored
(e.g. `_teacher`, `_standard`).

### Polymorphic Types

Prefer short, single-letter names for polymorphic type variables and arguments.
You'll notice a number of conventions with type variables.

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

These are conventions only, but picking something unusual for a common typeclass
might surprise other readers.

### Concrete Types

Avoid using single-letter names or abbreviations for values with a concrete
type, especially if that type is domain-specific. To crib from the
[Swift API Design Guidelines](https://swift.org/documentation/api-design-guidelines/#fundamentals),
"Clarity is more important than brevity."

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

We tolerate a few abbreviations as components of identifiers (e.g. the `num`
portion of `numQuestionsAnswered`), but there doesn't seem to be any broad
consensus about this.

### Database Access

We use the prefix `fetch` to signal retrieving data from the database.

Don't do this:

```
getAnswers :: SqlReadT [Answer]
```

Do this:

```
fetchAnswers :: SqlReadT [Answer]
```

### Database Entities

Use [`mkPersist`](mkPersist) from [`persistent`](persistent) to generate record
types corresponding to database tables. [`sqlSettings`](sqlSettings) will
autoprefix each field with the record's name. Note that the field names given
in the `persistLowerCase` quasiquotation should exactly match the corresponding
column name in the database table except that the former is `camelCase` and the
latter is `snake_case` (see [Postgres](#postgres) above). For example:

```haskell
share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
User sql=users
  name Text
  age Natural
  deriving Eq Show Ord Generic
|]
```

will generate a data declaration that looks like this:

```haskell
data User = User
  { userName :: Text
  , userAge :: Natural
  }
  deriving (Eq, Show, Ord, Generic)
```

Entities that are meant to appear in API requests or responses should have JSON
instances that strip the prefixes:

```haskell
instance ToJSON User where
  toEncoding = genericToEncoding (unPrefix "user")
  toJSON = genericToJSON (unPrefix "user")

instance FromJSON User where
  parseJSON = genericParseJSON (unPrefix "user")
```

This will produce the following JSON:

```haskell
{ "name": "Joe"
, "age": 29
}
```

### Request/Response types

For complicated `Handler` requests or responses, where we use a custom type
instead of domain object, such types should be named `{Resource}{Method}` where
`Resource` matches the route and `Method` is Sentence-case.

```hs
-- Good
data SchoolGet
  -- ...

instance ToJSON SchoolGet where
  -- ...

getSchoolR :: SchoolId -> Handler Value
getSchoolR schoolId = do
  school <- get404 schoolId
  sendStatusJSON status200 SchoolGet {- ... -}

data SchoolsPost
  -- ...

instance FromJSON SchoolsPost where
  -- ...

postSchoolsR :: Handler Value
postSchoolsR = do
  SchoolsPost {..} <- requireJsonBody
  -- ...

-- Bad
data GetSchool
  -- Out of order (though gramatically attractive)

data SchoolGET
  -- Wrong case

data SchoolResponse
  -- Missing Method, redundant suffix

data GetSchools
  -- Incorrect plurization

data CreateSchoolPost
  -- Duplicate verb, incorrect pluralization
```

## JavaScript

JavaScript uses `camelCase` for identifiers and `TitleCase` for classes and
React components. For flow types, we generally append a `T` to the end of the
type, e.g. `SkillT`. Names that are being ignored should be prefixed with an
underscore (e.g. `_teacher`, `_standard`). It's a common mistake (especially for
Haskellers) to use a lone `_` and accidentally overwrite underscore in that
scope.

## {DRAFT} Interoperation Between Languages

JSON keys should always be `camelCase`. String enums that cross language
barriers should be `snake_case`, since most enums that cross language barriers
are designed for `postgres` first. Exceptions can be made for short initialisms
which may be `ALLCAPS` (e.g. `TEKS: MathSystemT`). Short initialisms that are
part of other identifiers should use `snake_case`
(e.g. `rti_coordinator: TeacherRoleT`).

### Generating Enums from Haskell for Postgres

Snake-case enum

```haskell
data TeacherRole
  = Teacher
  | RtiCoordinator
  | ...

-- Equivalent to
--   mkPersistEnumUsing (snakeCaseify . unCapitalize) ''TeacherRole
-- Produces
--   'teacher'
--   'rti_coordinator'
--   ...
mkPersistEnum ''TeacherRole
```

All-caps enum

```haskell
data MathSystem
  = CCSS  -- Common Core Standard System
  | TEKS  -- Texas Essential Knowledge and Skills
  | ...

-- Produces
-- 'CCSS'
-- 'TEKS'
mkPersistEnumUsing id ''MathSystem
```

### Generating Enums from Haskell for JavaScript

Snake-case enum

```haskell
data TeacherRole
  = Teacher
  | RtiCoordinator
  | ...

-- Equivalent to
--   deriveJSONEnumUsing (snakeCaseify . unCapitalize) ''TeacherRole
-- Produces
--   "teacher"
--   "rti_coordinator"
--   ...
deriveJSONEnum ''TeacherRole
```

All-caps enum

```haskell
data MathSystem
  = CCSS  -- Common Core Standard System
  | TEKS  -- Texas Essential Knowledge and Skills
  | ...

-- Produces
-- "CCSS"
-- "TEKS"
-- ...
deriveJSONEnumUsing id ''MathSystem
```

### Enums in JavaScript

JavaScript enums should be typed using `flow`, e.g.:

```jsx
type TeacherRoleT
  = 'teacher'
  | 'rti_coordinator'
  | ...
```

One is encouraged to make smart constructors for enums to reduce the risk of
misspellings. `flow` usually catches misspellings, but not always.

```jsx
const TeacherRoles = {
  Teacher: ('teacher': TeacherRoleT),
  RtiCoordinator: ('rti_coordinator': TeacherRoleT),
  ...
}

-- e.g.
const role = TeacherRoles.Teacher
```

[mkPersist]: https://www.stackage.org/haddock/lts-12.0/persistent-template-2.5.4/Database-Persist-TH.html#v:mkPersist
[persistent]: http://hackage.haskell.org/package/persistent
[sqlSettings]: https://www.stackage.org/haddock/lts-12.0/persistent-template-2.5.4/Database-Persist-TH.html#v:sqlSettings
