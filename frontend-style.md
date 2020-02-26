# Frontend Style Guide

## Required tooling 

These automated tools will warn and/or automatically fix most issues with
syntax-related style. Any of our rules subsumed by one of these tools is not
documented here.

- [eslint](https://eslint.org/) for JS/JSX
- [stylelint](https://stylelint.io/) for CSS/SASS/SCSS
- [prettier](https://prettier.io/) for JS/JSX autoformatting

Integrating these tools into your preferred editor will make your life easier,
since CI will not pass without a warning-free codebase.

## File organization

All files should be organized by feature (e.g. math standard practice) instead of function (e.g. backbone collection). This includes:

- Backbone models & collections
- Components
- Jest tests
- Data mocks for jest tests

Files pertaining to a single component specific to a feature should be organized as follows:

```
.
└── my-page
    ├── index.js
    └── my-component
        ├── index.js
        ├── my-component.js
        ├── my-component.scss
        └── my-component.test.js
```

- `my-page/index.js` is a "handler" style component responsible for external
  API calls
- `my-component/index.js` is a "container" style component responsible for
  IO operations like global state management, user input or responding to
  timers
- `my-component/my-component.js` is a "presentational" style component with
  only completely isolated, local state.
- `my-component/my-component.test.js` contains jest tests for `my-component`
- `my-component/my-component.scss` contains `scss` rules for `my-component`
  that can be imported using `css modules`

`my-page` should export a `React` class named `MyPage` to be referenced in the
project's central `router`.

## Formatting

Use [`prettier`](https://prettier.io/) for autoformatting. This way, we don't need to define a pedantic
formatting style; we just let `prettier` decide for us.

### JS Style Rules

#### Variables

Avoid variable mutation as much as possible.

Use `const` by default.  If you must reassign a variable, use `let`. Never use
`var`.

```js
// bad, name is not reassigned
let name = "Doctor Brown"
console.log({name}}

// bad, var is outlawed
var name = "Doctor Brown"
console.log({name}}

// good
const name = "Doctor Brown"
console.log({name})

// fine, name is reassigned
let name = "Doctor Brown"
name = name.split(" ")
console.log({name}}
```

#### Objects

Use literal syntax to construct objects.

```js
// bad
const item = new Object()

// good
const item = {}
```

#### Arrays

Use array literal syntax to construct arrays

```js
// bad
const items = new Array();

// good
const items = []
```

## JSX Style Rules

### Stateless Components

Prefer stateless components when possible.

```jsx
// bad
class Link extends React.Component<void, Props, void> {
 render(): React.Node {
  return (
    <a href="#foo">Link</a>
  )
 }
}

// good
function Link(props: Props): React.Node {
  return <a href="#foo">Link</a>
}
```

### Component isolation

Every file should export exactly one component. Private sub-components may be
colocated as long as they are not exported.

```jsx
// bad, Image and Gallery can be both be imported from other modules
export class Gallery extends React.Component<GalleryProps, GalleryState> {
 ...
 render(): React.Node {
  return (
    <div>
      <Image src="foo.png" />
    </div>
  )
 }
}

export function Image(props: ImageProps): React.Element<any> {
  return <img src={props.src} />
}

// Good, StudentAdaptiveRow is private to StudentAdaptiveResultTable
export class StudentAdaptiveResultTable extends React.Component<StudentAdaptiveResultTableProps, StudentAdaptiveResultTableState> {
 ...
 render(): React.Node {
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

function StudentAdaptiveRow(props: StudentAdaptiveResultRowProps): React.Node {
  return (
    <tr>
      <td>{props.name}</td>
      <td>{props.accuracy}</td>
      <td>{props.date}</td>
    </tr>
  )
}
```

### Import structure

Assume this directory tree for the following examples:

```
.
└── my-other-page
    └── index.js
└── my-page
    ├── index.js
    ├── my-page.scss
    └── my-component
        ├── index.js
        ├── my-component.js
        ├── my-component.scss
        └── my-component.test.js
```

Imports for component-local files should be relative. non-local imports should
be absolute:

```js
// my-component/index.js

// bad

import {MyComponent} from '@example/src/my-page/my-component'

import {MyOtherPage} from '../../my-other-page'
import Style from '../my-page/my-page.scss'

// good

import {MyOtherPage} from '@example/src/my-other-page'

import {MyComponent} from './my-component'
import Style from '../my-page/my-page.scss'
```

## Layout & CSS

### Laying out a grid

Avoid bootstrap grid in favor of flexbox:

```jsx
// bad

<div className="row">
  <div className="col-xs-4">Column 1</div>
  <div className="col-xs-4">Column 2</div>
  <div className="col-xs-4">Column 3</div>
</div>

// good

<div className="componentWrapper">
  <div className="column">Column 1</div>
  <div className="column">Column 2</div>
  <div className="column">Column 3</div>
</div>

.componentWrapper { 
  display: flex;
  
  .column { 
    flex: 1;
  }
}
```

### Sass

#### Colors

Source colors from [`variables/colors.scss`](https://github.com/freckle/megarepo/blob/master/frontend/fr-materials/css/variables/colors.scss).

```scss
// bad

.orangeThing {
  color: #f54040;
}


// good

@import '@fr-materials/css/style.scss';

.orangeThing {
  color: $sunset-orange;
}
```

Color names are generated using the [Name That Color Tool](http://chir.ag/projects/name-that-color/).
It should be rare to add a new color to the site. _All new colors should be
verified by a designer._

#### Margin & Padding

Source margin and padding values from
[variables/settings.scss](https://github.com/freckle/megarepo/blob/master/frontend/fr-materials/css/variables/settings.scss).

```scss
// bad

margin: 8px;

// good

@import '@fr-materials/css/style.scss';

margin: $std-margin-sm;
```

#### CSS class names

Use [semantic CSS class names](https://css-tricks.com/semantic-class-names/).

```scss
// bad

.italic {
  font-style: italic;
}

.thin {
  font-weight: 100;
}

// <div class="italic thin">Copyright 2020 Renaissance Inc <div/>

// good

.copyright {
  font-style: italic;
  font-weight: 100;
}

// <div class="copyright">Copyright 2020 Renaissance Inc <div/>
```
