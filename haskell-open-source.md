# Best Practices

1. If authoring a distinct package of reasonable size and generic utility,
   prefer starting a separate, public repository utilized as a Git dependency.
   A Haskell library template includes common setup for tooling and actions:
   `gh repo create --template freckle/haskell-library-template`

When working in a public repository:

1. Keep a linear Git history with rich, meaningful commit messages

   Configure GitHub to only allow Squash and Rebase strategies.

1. Maintain a professional atmosphere in Issue and Pull Request comments
1. Release to Hackage and Stackage once libraries become stable
1. Use proper versioning for released packages (see below)

## Licensing

All open source projects under the `freckle/` organization must use an MIT
license with "Renaissance Learning Inc" as the copyright holder.

Example:

```
The MIT License (MIT)

Copyright (c) {Year} Renaissance Learning Inc

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## Versioning

Released packages should use a version string of:

```
EPOCH.MAJOR.MINOR.PATCH
```

Where

1. A rewrite or major shift in API happens in `EPOCH`
1. Breaking changes may only happen in `MAJOR` (or `EPOCH`)
1. New (non-breaking) API additions happen in `MINOR`
1. Bug fixes that don't effect the API happen in `PATCH`

To determine what changes are "breaking", use the following:

![](./pvp-chart.png)

With `A=EPOCH`, `B=MAJOR`, and `C=MINOR`.
