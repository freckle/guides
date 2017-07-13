# Front Row Frontend

This document is meant to give a broad overview of the state of the frontend codebase of FrontRow.

## Applications

All the frontend codebase exist in [`megarepo/frontend`](https://github.com/frontrowed/megarepo/tree/master/frontend).

The frontend is split into 3 different applications:
  - [Teacher Dashboard](https://classroom.frontrowed.com)
  - [Student Dashboard](https://student.frontrowed.com)
  - [School Dashboard](https://school.frontrowed.com)

All applications use the same stack; share the same tools, build processes, guidelines, and architecture; and produce the same type of output.

We try to share code common to several applications as much as we can, and move it to the folder [`frontend-entities`](https://github.com/frontrowed/megarepo/tree/master/frontend/frontend-entities).

Teacher and Student applications are both [Single Page Apps](https://en.wikipedia.org/wiki/Single-page_application).
The first loading of the web app loads the Javascript bundle, the CSS bundle, vendor files (bundled or individual) and other 3rd party services (Google analytics, Intercom, MathJax, ...).
After the first load, all interactions and route change will not trigger a page reload unless on very explicit operation (logout, change school, ...).

## Browser Compatibility, Devices and resolutions

### Browser Compatibility

For the teacher dashboard all users visit the website inside the browser using Chrome, Safari, Firefox and IE.
We **officially support Chrome and Safari**.
We have to support Firefox and IE > 9, but don't officially support them.
Users using IE will see a banner explicitly saying that we don't support IE.

_Warning: Some part of the web apps will not work in IE 9 (word study drag and drop, ...)_

### Devices and resolutions

#### Teacher Dashboard

For the teacher dashboard, we optimize for a desktop resolution (>= 1366x768) but support iPad (1024x768).
Interactions should account for touch devices and mouses/trackpads.

_Browser and devices for Teacher Dashboard during the school year 2016-2017 (source: Google Analytics)_

| Browser         | Users |
| --------------  |:-----:|
| Chrome          | 65%   |
| Safari          | 20%   |
| Safari (in-app) | 6%    |
| Firefox         | 5%    |
| IE              | 3%    |

| Screen Size     | Users |
| --------------  |:-----:|
| 1366x768        | 27%   |
| 1024x768        | 26%   |
| 1280x800        | 9%
| 1440x900        | 9%    |
| 1280x1024       | 6%    |
| 1920x1080       | 4%    |

#### Student Dashboard

For the student dashboard, we optimize for a iPad (1024x768) and ChromeBook (1366x768) sizes.
All interactions should support using a touch device
Trackpads on ChromeBook are not easy to master and our users have difficulties being precise with it. That's why a lot of ChromeBook users cannot use the drawing features in Math questions.

_Browser and devices for Student Dashboard during the school year 2016-2017 (source: Google Analytics)_

| Browser         | Users |
| --------------  |:-----:|
| Chrome          | 65%   |
| Safari (in-app) | 17%   |
| Safari          | 11%   |
| IE              | 3%    |
| Firefox         | 2%    |


| Screen Size     | Users |
| --------------  |:-----:|
| 1366x768        | 50%   |
| 1024x768        | 30%   |
| 1280x800        | 3%
| 1440x900        | 3%    |


#### iOS App

Front Row also have a [native iOS application](https://itunes.apple.com/us/app/front-row-math-english-language-arts/id708421745?mt=8) on the Apple Store.
The native app is does contains 3 screens:
- A Menu page allowing user to chose between `Student Dashboard` and `Teacher Dashboard`
  - Clicking on `Teacher Dashboard` will open the Teacher Dashboard in Safari
  - Clicking on `Student Dashboard` will launch the loading screen
- The loading screen: Just a progress bar waiting for the resources of the `Student Dashboard` to load
- The Student Dashboard: This is just a [webview](https://developer.apple.com/documentation/webkit/wkwebview) containing the web version of the student dashboard

Dev Mode: The native app developers to pick a different environment than production for testing purpose.
To do that you have to tap the **o** in the title `Row` 5 times, an alert will show up to pick up the environment you want.

## Build

All build steps are written in a bash file at the root of each project called `build.sh`.
It contains all the steps to create the different output of each web application.
The build usually includes:
- Converting SASS to CSS
- Bundling CSS
- Versioning CSS bundle
- Running Flow type check
- Running ESLint
- Bundling JS
- Uglyfying JS bundle
- Versioning JS bundle
- Bundling JS vendor files
- Uglyfying JS vendor bundle
- Versioning JS vendor bundle
- Running the test

### Setting the project

We have multiple scripts to help setting the frontend.

**Main setup all**

```
cd megarepo/frontend
./setup.sh
```

This will install all the dependencies for all project without building each one of them.

**Main build all**

```
cd megarepo/frontend
./build-all.sh
```

This will run the build for each individual project but will assume the project was setup successfully. It may take some time.

**Reset all**

```
cd megarepo/frontend
./reset-all.sh
```

This will remove the `node_modules` folder of each project, re-install AND build every project individually. This is useful when something went wrong in your repos and you need a clean blank state.

### Running the build

To run the build or part of the build, we use NPM scripts. They are the same for all web applications.

**Build for dev environment**
```
yarn build:dev
```

**Build for prod environment**
```
yarn build:prod
or
yarn build
```

**Launch the watcher and rebuild for dev after each local change**
```
yarn watch
```

**Run ESLint and stylelint with all warnings**
```
yarn lint
```

Code Climate, running on each PR, will track and indicate linting (CSS and JS) warnings.


### Internal dependencies

In order to share code of `frontend-entities` between multiple projects, we import the folder as a package.


### Main tools used for the build


#### Browserify

We use Browserify to create JS bundles of our code and vendor files. We use a lib on top of Browserify called [`browserify-incremental`](https://github.com/jsdf/browserify-incremental) allowing us to speed us the bundling process by not re-creating the bundle from scratch after every change.
Each build type (`dev` or `prod`) uses a different cache file for this process (`browserify-cache-development.json`).
Browserify is supplemented by [`babelify`](https://github.com/babel/babelify) which compile our ES6 code into browser compatible Javascript (learn more about [Babel](http://babeljs.io/]).

#### Flow

See Stack/Flow


## Stack

The stack of the frontend is almost the same for each project with few exceptions.

### Backbone Collections and Models

When calling an API resource, we use Backbone Collections and Models to transform them into usable, type-checked, javascript object.
Every resource is defined by it's model. Each model contains a `parse` function validating and transforming data from API response.

_Example of the model parse function_
```javascript
parse: function(response: Object): Object {
  // Validating the API response contains the keys and values we expect at runtime
  const model = recordTS(
    { id: numberTS
    , name: stringTS
    , createdAt: dateTS
    , completedAt: nullableTS(dateTS)
    , studentIds: arrayTS(numberTS)
    }
  )
  const newResponse = objectOfType(response, model)
  // Converting date from string to moment
  newResponse.createdAt = moment(newResponse.createdAt)
  newResponse.completedAt = newResponse.completedAt ? moment(newResponse.completedAt) : null
  return newResponse
}
```

To add more type safety around Backbone Model we add some boilerplate around the model creation.
For each model we define the type of the return of the `get` function for each valid key. Calling `get` on a non valid `key` should throw a flow error at compile time, same for using the value as the incorrect type.
To achieve this we have to define the type of the `Get` function as followed.
From the same model as above:

```javascript
type Get =
  ((attr: 'id') => number) &
  ((attr: 'name') => string) &
  ((attr: 'createdAt') => moment) &
  ((attr: 'completedAt') => ?moment) &
  ((attr: 'studentIds') => Array<number>)
```

And attach the type of the `Get` function to the Backbone Model:

```javascript
export type MyModelT = Backbone.Model<Get> & typeof instanceMethods
const MyModel: Class<Backbone.Model<Get>> = Backbone.Model.extend(instanceMethods)
export default MyModel
```

Backbone Collections follow the same pattern.

```javascript
const instanceMethods =
  { url: function(): string {
      return CApiHelper.fancyPaths.v2....[your resource path]
    }
  , model: MyModel
}

const MyCollection: Class<Backbone.Collection<MyModelT>> = Backbone.Collection.extend(instanceMethods)
export type MyCollectionT = Backbone.Collection<MyModelT> & typeof instanceMethods
export default MyCollection
```

More info about [Backbone])http://backbonejs.org/). You can also check the [annotated sources of Backbone](http://backbonejs.org/docs/backbone.html)

### Backbone Router

We use Backbone Router to handle the browser history and change of URL. Each route is defined in the `routes` object and attached to its handler.
We try to put handler in separate file specific to each section/product (example: `ela-handler.js`, `math-handler.js`, ...).

We supplement Backbone Router with 2 extensions (as vendor files) to allow using and understanding query string in URL (not supported by default):
- backbone.customhistory.js
- backbone.queryparams.js

Each handler is responsible for fetching the resources required for the component.

To avoid fetching a resource that we already have we use a global state. The global state is instantiated in the Backbone Router. When doing a fetch on a collection already populated, it will not launch the request again.

To force the re-fetching use:
```javascript
myCollection.fetch({reload: tue})
```

This behavior is not in the default Backbone Router but comes from the extension `backbone.cacheit.js` imported as a vendor file.

### Flow

[Flow](https://flow.org/) is a tool that add a layer of typing to javascript code. It relies on annotations in the code to determine the types of expressions. When running `flow check`, flow will check the code base and throw all typing errors found. Flow contains the definition of the javascript API and can be completed by passing definition of libraries we use, like React, Backbone or Lodash. Other library definition can be in [flow-typed](https://github.com/flowtype/flow-typed).
Importing a name into a module imports its type but you can chose to only import its type.

Flow is not perfect but improving. We try to stay up to date with new Flow versions.
When using Flow and facing a invalid Flow error, you can use `$FlowFixMe` to remove flow type checking of the next line. Every time you use this, you should make sure you will refactor this part.

### React

TODO [React](https://facebook.github.io/react/)

### Mocha and Chai

See Testing

### 3rd Party libraries

TODO

- MathJax
- TrackJS
- Tracking and marketing
  - Google Tag Manager
  - Facebook Pixel Tracking
  - Twitter (embedded)
  - Youtube (embedded)


## Best Practice and syntax


### Javascript Style

Refer to the [Javascript style document](https://github.com/frontrowed/guides/blob/master/javascript-style.md)

### Linting

We use 2 tools for linting the frontend code:
- [ESLint](http://eslint.org/) for Javascript
- [Stylelint](https://github.com/stylelint/stylelint) for CSS and SASS

Linting is included in all type of builds (`dev` and `prod`). Linting errors will break the build and linting warnings will be reported by Code Climate or can be seen when running `yarn lint`.

The ESLint and stylelint configuration is a work in progress, we frequently add, update or remove rules to make the code more homogeneous and readable.
Some rules will throw errors to prevent them from being in the code, others will just throw warnings to allow developers to prototype rapid solutions.
We expect PRs to not contain any linting warnings.


## Testing

We use [mocha](https://mochajs.org/) as the JS test framework and [chai](http://chaijs.com/) as the assertion library.

We expect developers to add tests to logic and helper files.

All tests exist in the `test` folder of every application. We keep the same name and file path of the tested file for the test file:
`js/helpers/my-nice-helper.js` should have a corresponding test file: `test/helpers/my-nice-helper-test.js`.

By convention the first `describe` should contain the name of the tested file. Every nested `describe` should contain the name of the tested function.
Example:

```javascript
describe("my-nice-helper", function() {

  describe("myFunction", function() {

    it("should do this", function() {
      ...
    })

    it("should not do this", function() {
      ...
    })
  })
})
```

We don't have any tool yet to measure test coverage.

We currently don't test React component.


## RoadMap

This is the non-exhaustive list of points we would like to implement short, medium and long term.
This list is ordered sequentially, some points can be done without the preceding ones but it is mostly in the order we want to do them.

* Update all Backbone Models and Collections with correct Flow syntax
* "Virtually" remove every object from the window object (Backbone, underscore, jQuery)
* Move vendor files to npm package (Backbone, lodash, JQuery, ...)
* Update critical libraries (lodash, Backbone, React, ...)
* Replace Browserify with Webpack
* Change Architecture of the code to be split by modules and features
```
Ela
| adaptive
| | report
| | | collections
| | | models
| | | components
| | | logic
```
* Effectively measure Flow coverage and improve this metric
* Effectively measure test coverage and improve this metric
* Replace JQuery Ajax with fetch API
* Remove code using JQuery for DOM manipulation
