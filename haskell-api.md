# Haskell API Conventions

## Stateless

Prefer stateless endpoints. Sessions are used to authorize data access, but not
choose what data to return.

Good:

- `/3/students?schools.id=x,y,z`
- `ApiClient.fetch("/3/students", { "schools.id": me.schoolIds })`

Bad:

- `/3/school-admins/me/students`
- `ApiClient.fetch("/3/school-admins/me/students")`

## Parameters

Prefer filtering by query parameter instead of making distinct endpoints
filtered by their path-pieces.

Good:

- `/3/students?schools.id=x,y,z&teachers.id=a,b,c`

Bad:

- `/3/schools/:id/students`
- `/3/teachers/:id/students`
- `/3/schools/:id/teachers/:id/students`

### Operations

For complex filtering, follow an "operation suffix" syntax,

Good:

- `status[in]=draft,review`
- `completed-at[gt]=...&completed-at[lte]=...`

Bad:

- `status=draft&status=review`
- `from=...&to=...`

Endpoints should support parameters without an operation suffix like `[in]`.

### Naming

Filters should match the dotted-path of the filtered attribute in the response,
to aid in discoverability and consistency.

Parameters must be hyphen-case (despite fields being camelCase in responses) to
account for that fact that URLs are case-insensitive.

For example, given:

```
/3/teachers
[ { id:
  , ...
  , school:
    { id:
    , createdAt:
    , ...
    }
  }
]
```

Here, a School filter would be `school.id` or `school.created-at`.

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

### `PATCH` semantics

Modifications to an existing resource should be made via a `PATCH` request.

### Specific guidelines

A well-formed `PATCH` endpoint

- Must update an entity's `updatedAt` field, if present.
- Should return the modified resource. The resource should be re-fetched from
  the database for ease of testing and to ensure the update was correctly
  persisted.
- Must accept `null` to unset nullable fields, disallowing it for
  non-nullables.
- Must 404 if a resource with the route's identifier(s) does not exist.
- Must not create resources (that's the role of `POST`).
- Must allow for fields to be optional, even allowing for entirely empty `PATCH`
  objects.

#### Structural similarities to `GET`

As a rule-of-thumb for a given resource, `PATCH` requests share a similar shape
to `GET` requests in that

- they should have the same route (e.g. `/3/teachers/7654` would `GET` or
  `PATCH` the teacher with id `7654`),
- `PATCH` objects should resemble the fields yielded by the corresponding `GET`,
  albeit `PATCH` fields are (1) optional, (2) may allow additional fields or (3)
  disallow the modification of certain fields (e.g. `id`, `createdAt`), and
- if a `PATCH` response is non-empty, it should be the same payload that would
  be returned by a call to the corresponding `GET`.

#### Example

For example, assuming `GET /3/teachers/7654` yields

```json
{
  "id": 7654,
  "givenName": "John",
  "surname": "Kimble",
  "email": "jk@example.com",
  "phoneNumber": "555-555 5555",
  "addressLines": ["1234 Hollywood Dr., Hollywood, CA"],
  "administrativeArea": "CA",
  "country": "USA",
  "gradesTaught": ["K"],
  "createdAt": "2021-11-10T15:29:16.239Z",
  "updatedAt": "2021-11-10T15:29:16.239Z"
}
```

`PATCH /3/teachers/7654` could accept the following update payloads

```javascript
// Change the teacher's name
{
  "givenName": "Arnold",
  "surname": "Schwarzenegger"
}

// Remove the teacher's phone number
{
  "phoneNumber": null
}

// Updates `updatedAt`
{
}

// Updates `updatedAt` (immutable fields and unsupported fields are ignored)
{
  "createdAt": "2019-11-10T15:29:16.239Z"
}
```

However, `PATCH /3/teachers/7654` would fail given the following payloads

```javascript
// BAD REQUEST, trying to unset a required field
{
  "email": null
}

// BAD REQUEST, validation found `ZZ` is not within `USA`
{
  "addressLines": ["1234 Hollywood Dr., Hollywood, ZZ"],
  "administrativeArea": "ZZ",
  "country": "USA"
}
```

## Status Codes

- 201 for creation
- 202 for [Long running operations][long-running-operations]
- 204 for no content (i.e. our `Empty` responses)
- 400s MUST follow our `ValidationError` machinery

[long-running-operations]: https://github.com/microsoft/api-guidelines/blob/vNext/Guidelines.md#13-long-running-operations

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

## API Types

Prefer distinct types for requests and responses that are only used for that. We
call these "API Types". Do not use persistent Entity types as API request or
response types.

1. A response API Type should be named `Api{Resource}` and should be re-used
   anywhere `Resource` appears in the API.

   If the same concept takes on a different shape in different places, it should
   be considered a different Resource, with a different name. For example,
   `ApiTeacher`, `ApiAbbreviatedTeacher`, `ApiTeacherWithSchool`,
   `ApiUsageReportTeacher`.

1. A request API Type should be named `Api{Action}{Resource}({Target})`.

   ```hs
   -- Good (Target omitted, acting on entire resource)
   data ApiCreateTeacher
   data ApiDeleteTeacher

   -- Good (Target included)
   data ApiSetTeacherPassword

   -- Bad
   data TeacherPUT
   data CreateApiTeacher
   data ApiTeacherSetPassword
   ```

1. An API Type should not be exported unless it is shared and in its own module

   This is because using un-prefixed record fields, and simple deriving of
   instances, is preferred. Keeping it local or in its own dedicated module can
   prevent collision-related problems.

## Module Structure

- Prefer in-module data-access functions, until they require sharing
- Order your module as:
  - Request/response type(s)
  - Handler that uses it
  - Repeat if more than one handler
  - Internal functions
- Document your endpoints

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

For asserting against JSON bodies while ignoring extra fields or ordering, use
[hspec-expectations-json][].

[hspec-expectations-json]: https://hackage.haskell.org/package/hspec-expectations-json
