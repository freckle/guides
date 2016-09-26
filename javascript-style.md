# Front Row JavaScript Style Guide

## Comma-First style

```javascript
// bad
const t =
  [ a,
    b,
    c
  ]
return (
  { a : "ape",
  b : "bat"
  }
)

//good
const t =
  [ a
  , b
  , c
  ]

return (
  { a : "ape"
  , b : "bat"
  }
)
```

## References

Use `const` for all of your references; avoid using `var`.

```javascript
// bad
var a = 1;
var b = 2;

// good
const a = 1;
const b = 2;
```

If you must reassign references, use `let` instead of `var`.

```javascript
// bad
var count = 1;
if (true) {
  count += 1;
}

// good, use the let.
let count = 1;
if (true) {
  count += 1;
}
```

## Objects


Use the literal syntax for object creation.
```javascript
// bad
const item = new Object();

// good
const item = {};
```

Use property value shorthand

```javascript
// bad
const user = { firstname: firstname }

// good
const user = { firstname }
```

Only quote properties that are invalid identifiers.
```javascript
// bad
const user =
  { "firstname": "Jeff"
  , "age-group": "old"
  }

// good
const user =
  { firstname: "Jeff"
  , "age-group": "old"
  }
```

## Arrays

Use the literal syntax for array creation.

```javascript
// bad
const items = new Array();

// good
const items = []
```

## Mutation

Don't mutate arrays

```javascript
// bad
const items = []
items.push("foo")

// good
const items = [ "foo" ]

// good, use concat
const items = [ "foo" ]
const additionalItems = [ "bar" ]
const newItems = items.concat(additionalItems)
```

Don't mutate objects

```javascript
// bad
const user = {}
user.firstname = "Jeff"

// good
const user =  { firstname: "Jeff" }


// bad
function addAge(user: Object, age: number): Object {
  user.age = age
  return user
}

// good with vanilla JS
function addAge(user: Object, age: number): Object {
  return Object.assign({}, user, {age})
}
// good with lodash
function addAge(user: Object, age: number): Object {
  return _.extend({}, user, {age})
}
```

## Stateless components


Use stateless component when possible.


```javascript
// bad
class Link extends React.Component<void, LinkProps, void> {
 render(): React.Element<any> {
  return (
    <a href="#foo">Link</a>
  )
 }
}

// good
const Link = function(props: LinkProps): React.Element<any> {
  return <a href="#foo">Link</a>
}
```

## Return statements

If your return statement spill over multiple lines use parentheses.

```javascript
// bad
return <div>
  Foo
</div>

// good
return (
  <div>
    Foo
  </div>
)

// bad
return {
  name: "Jeff"
}

// good
return (
  {name: "Jeff"}
)
// also good
return {name: "Jeff"}
```

## Component splitting

Each React component should live in its own file. This rule only apply to stateful components. If you have a component that uses small stateless components specific to the main component you can put them in one file.

```javascript
//bad
const Image = function(props: ImageProps): React.Element<any> {
  return <img src={props.src} />
}

class Gallery extends React.Component<void, GalleryProps, GalleryState> {
 ...
 render(): React.Element<any> {
  return (
    <div>
      <Image src="foo.png" />
    </div>
  )
 }
}
// Here the `Image` component is highly reusable and should be moved to its own file

//good
const StudentAdaptiveResultRow = function(props: StudentAdaptiveResultRowProps): React.Element<any> {
  return (
    <tr>
      <td>{props.name}</td>
      <td>{props.accuracy}</td>
      <td>{props.date}</td>
    </tr>
  )
}

class StudentAdaptiveResultTable extends React.Component<void, StudentAdaptiveResultTableProps, StudentAdaptiveResultTableState> {
 ...
 render(): React.Element<any> {
  return (
    <table>
      <StudentAdaptiveResultRow
        name={name}
        accuracy={accuracy}
        date={date}
      />
    </table>
  )
 }
}
```
0
