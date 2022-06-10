# Best Practices

1. If authoring a distinct package of reasonable size and generic utility,
   prefer starting a separate, public repository utilized as a Git dependency.

   Templates may exist for common cases, such as a [Haskell library][gh-hlt], or
   a [TypeScript GitHub Action][gh-tga]. If one doesn't exist, consider making
   one and then using it to create your package.

   [gh-htl]: https://github.com/freckle/haskell-library-template
   [gh-tga]: https://github.com/freckle/typescript-action-template

When working in a public repository:

1. Keep a linear Git history with rich, meaningful commit messages

   Configure GitHub to only allow Squash and Rebase strategies.

1. Maintain a professional atmosphere in Issue and Pull Request comments
1. Formally release early (e.g. to NPM or Hackage/Stackage), as soon as the
   library becomes stable
1. Use proper versioning for released packages

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
