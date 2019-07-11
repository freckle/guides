# Haskell API Conventions

## Stateless

Prefer stateless endpoints. Sessions are used to authorize data access, but not
choose what data to return.

- Good:
  - `/3/students?schools.id=x,y,z`
  - `ApiClient.fetch("/3/students", { "schools.id": me.schoolIds })`
- Bad:
  - `/3/school-admins/me/schools`
  - `ApiClient.fetch("/3/school-admins/me/schools")`

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

- If filtering on an attribute of the main resource: `{attribute}={value}`
- If filtering on an attribute of a related resource:
  `{resource}.{attribute}={value}`
- If one thing has many of another, pluralize the many

Examples, for a `/teachers` endpoint:

- `?name=`, because a Teacher has one name
- `?school.id=`, because School is a relation, and a Teacher has only one
- `?students.id=`, because Student is a relation, and a Teacher has many
- `?grades=`, direct and has many

Note that pluralization can occur in either position. For example, Teachers have
many Students, and (if we assume for the sake of this of this example) Students
have many levels, then you would filter Teachers by their Students' levels with
`students.levels=`.

### Semantics

Prefer `IN` equality semantics.

- Good: `{attribute}={value1},{value2},{value3},...`
- Bad: `{attribute}={value}`

This is because:

- It's just as easy to support as direct equality
- It reduces to the expected `EQUALS` semantics when given one value
- We think we can get by with only this level "smart" filtering for a while and
  defer more complexity in this area (e.g. "not", etc).

---

**Aside about URL length limits**: We don't know at what size this will be a
problem. Standards-wise, there is no limit, but browsers and servers do have
limits and it's recommended to use 2000 as a reference point [ref].

[ref]: https://stackoverflow.com/questions/417142/what-is-the-maximum-length-of-a-url-in-different-browsers

However, I will say this:

- Our current `/foo/:id,:id,:id/bar` does bump into issues of length and it is
  annoying to resolve because it means a new *route* to find a natural way to
  group the massive list (i.e. all students in a school will mean changing
  `/students/...` to `schools/:id`)
- Doing the same thing in a `GET` param will be much nicer to handle when we
  encounter it because we would probably already support both `students.id` and
  `schools.id` in the single endpoint and the Frontend can just switch

Therefore, I find the length concerns for `IN`-queries to be less problematic
than our multi-id path-pieces of today.

---

## Request Shape

- Request bodies can vary by use, but this should be avoided if possible

*TODO*: more to say here?

## Response fields

- Always include `id`s
- Within reason, choose attributes directly for the work that is motivating the
  endpoint
- Include related resources in abbreviated form, Frontend almost always wants
  names:
  - `students: [{id:, firstName:, lastName:}]`
  - `school: {id:, name:}`

## Status Codes

- 201 for creation
- 204 for no content (i.e. our `Empty` responses)
- 400s MUST follow our `ValidationError` machinery

## Pagination

- All list-returning routes of modest size should be paginated via `Yesod.Page`

## Modules Structure

- Fully expand all parents to start

  This means we won't break Frontend when the tree expands over time

- Fully name-space all parents and routes (see example)

  **Caveats**:

  - There's no need to `V3`-prefix everything
  - Things can "start over" when there's the "single below a list" situation,
    e.g. `TeacherP x $ TeacherR` instead of `TeachersP $ TeachersTeacherP x $
    TeachersTeacherR`, which is a bit buffalo-buffalo
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

  data TeacherPostBody

  -- Bad
  data Teachers

  getTeachersR :: Handler Teachers
  getTeachersR = undefined

  data PostTeacher
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
that passes the tests because it's used in both places. It also makes it clear
how to move to `aeson-lens`-based assertions (which are more robust) when that
makes sense.

```hs
-- Good
body <- getJsonBody
body `shouldBe` [object ["id" .= teacherId]]

-- Doesn't fail on unrelated attribute changes
body <- getJsonBody @Value
body
  ^.. _Array
  . traverse
  . key "id"
  . _JSON
  `shouldMatchList` [teacherId]

-- Bad
body <- getJsonBody
body `shouldBe` [TeacherGet teacherId]
```
