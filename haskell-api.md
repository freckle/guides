# Haskell API Conventions

## Stateless

Prefer stateless endpoints. Sessions are used to authorize data access, but not
choose what data to return.

- Good:
  - `/3/students?schools.id=x,y,z`
  - `ApiClient.fetch("/3/students", { "schools.id": me.schoolIds })`
- Bad:
  - `/3/school-admins/me/students`
  - `ApiClient.fetch("/3/school-admins/me/students")`

## Parameters

We want to :heart: query parameters as a way to keep the number of distinct
endpoints lower by supporting more complex filtering combinations within each
endpoints.

- Good: `/3/students?schools.id=x,y,z&teachers.id=a,b,c`
- Bad:
  - `/3/schools/:id/students`
  - `/3/teachers/:id/students`
  - `/3/schools/:id/teachers/:id/students`
  - etc, forever

### Naming

Filters should match the dotted-path of the filtered attribute in the response.

For example:

```
/3/teachers
[ { id:
  , ...
  , school:
    { id:
    , ...
    }
  }
]
```

Here, a School filter would be `school.id`: `/3/teachers?school.id=1,2,3`.

```
/3/students
[ { id:
  , ...
  , schools:
    [ { id:
      , ...
      }
    ]
  }
]
```

But here, it would be `schools.id`: `/3/students?schools.id=1,2,3`.

If a filter exists that is not for an attribute present in the response, the
name can be inferred by what it would look like if it were.

### Semantics

Prefer `IN` equality semantics.

- Good: `{attribute}={value1},{value2},{value3},...`
- Bad: `{attribute}={value}`

This is because:

- It's just as easy to support as direct equality
- It reduces to the expected `EQUALS` semantics when given one value
- We think we can get by with only this level "smart" filtering for a while and
  defer more complexity in this area (e.g. "not", etc).

## Response fields

- Always include `id`s
- Within reason, choose attributes directly for the work that is motivating the
  endpoint
- Include related resources in abbreviated form when Frontend needs extra data
  about them. For example:
  - `students: [{id:, firstName:, lastName:}]`
  - `school: {id:, name:}`

## HTTP Methods

### Prefer `PATCH` over `PUT`

`PUT` routes contain semantics that easily lead to bugs. We prefer `PATCH` when
possible. Issues include:

- The necessity of sending monolithic resources to a `PUT` when most uses are
  changing specific details.
- The ability to misinterpret an `undefined` as a `null` and have untintended
  consequences when evolving a handler. This is exacerbated by the default
  parsing behavior of `Maybe a` in `aeson`.
- The complexities that arise from extra validation or lack of validation
  especially when differing rules around mutability and roles arise.

If you are creating a new `PUT` consider if it could be expressed in a more
granular `PATCH` semantic.

## Status Codes

- 201 for creation
- 202 for [Long running operations][long-running-operations]
- 204 for no content (i.e. our `Empty` responses)
- 400s MUST follow our `ValidationError` machinery

[long-running-operations]:
  https://github.com/microsoft/api-guidelines/blob/vNext/Guidelines.md#13-long-running-operations

## Pagination

- All list-returning routes of modest size should be paginated via `Yesod.Page`

## Modules Structure

- Fully expand all parents to start

  This means we won't break Frontend when the tree expands over time

- Fully name-space all parents and routes (see example)

  **Caveats**:

  - There's no need to `V3`-prefix everything
  - Things can "start over" when there's the "single below a list" situation,
    e.g. `TeacherP x $ TeacherR` instead of
    `TeachersP $ TeachersTeacherP x $ TeachersTeacherR`, which is a bit
    buffalo-buffalo
  - The "start over" caveat does not apply to path naming (i.e. the above
    example will still have the module `Teachers/Teacher.hs`)

- Name modules to directly match parents and routes, which implies one Handler
  module per route (see example)

  **Caveats**:

  - In reality, we need to prevent collisions between versions through
    additional prefixing of route constructors. We ignore this in this guide.

Example:

```
/3 V3P:                         --> Handlers/V3
  /students StudentsP:          --> Handlers/V3/Students
    / StudentsR GET             --> Handlers/V3/Students.hs
    /#StudentId StudentP:       --> Handlers/V3/Students/Student
      / StudentR GET            --> Handlers/V3/Students/Student.hs
      /courses StudentCoursesP: --> Handlers/V3/Students/Student/Courses
        / StudentCoursesR GET   --> Handlers/V3/Students/Student/Courses.hs
```

## Module Structure

- Prefer in-module request/response types

  - Request type naming: `{Resource}{Method}Body`
  - Response type naming: `{Resource}{Method}`
  - NOTE: list responses are `[{Resource}{Method}]`, not `{Resources}{Method}`

  ```hs
  -- Good
  data TeacherGet

  getTeachersR :: Handler [TeacherGet]
  getTeachersR = undefined

  data TeacherPost
  data TeacherPostBody

  postTeacherR :: Handler TeacherPost
  postTeacherR = do
    body <- requireJsonBody @TeacherPostBody

  -- Bad
  data Teachers

  getTeachersR :: Handler Teachers
  getTeachersR = undefined

  data PostTeacher

  postTeacherR :: Handler (Entity Teacher)
  postTeacherR = do
    body <- requireJsonBody @PostTeacher
  ```

- Prefer in-module data-access functions, until they require sharing
- Order your module as:
  - Request/response type(s)
  - Handler that uses it
  - Repeat if more than one handler
  - Internal functions
- Document your endpoints

See `/3/teachers` as a good example.

## Testing

Do not export your request/response types for use in tests. Instead, re-build
JSON `Value`s with `object` This ensures you don't have a (de)serialization bug
that passes the tests because it's used in both places.

```hs
-- Good
body <- getJsonBody
body `shouldMatchList` [aesonQQ|
  [ { id: #{teacherId}
    }
  ]
|]

-- Or
body <- getJsonBody
body `shouldMatchList` [object ["id" .= teacherId]]

-- This doesn't fail on unrelated attribute changes
body <- getJsonBody @Value
body
  ^.. _Array
  . traverse
  . key "id"
  . _JSON
  `shouldMatchList` [teacherId]

-- Bad
body <- getJsonBody
body `shouldMatchList` [TeacherGet teacherId]
```
