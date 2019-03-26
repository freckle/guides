# Best Practices

1. If authoring a distinct package of reasonable size and generic utility,
   prefer starting a separate, public repository utilized as a Git dependency

When working in a public repository:

1. Keep a linear Git history with rich, meaningful commit messages

   Configure GitHub to only allow Squash and Rebase strategies.

1. Maintain a professional atmosphere in Issue and Pull Request comments
1. Release to Hackage and Stackage once libraries become stable
1. Use proper versioning for released packages (see below)

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
