## achille

[![View on Hackage](https://img.shields.io/hackage/v/achille?style=flat-square)][hackage]
[![Github Workflow](https://img.shields.io/github/workflow/status/flupe/achille/build?style=flat-square)][ci]

[hackage]: https://hackage.haskell.org/package/achille
[ci]: https://github.com/flupe/achille/actions/workflows/haskell.yml

> :information_source: The only version available on Hackage is largely outdated and shouldn't be used.
> This repository contains a WIP rewrite, based on [linear-smc][smc]. By defining `Recipe m` as 
> a cartesian category, it *should* be possible to provide a very intuitive
> EDSL syntax. Stay tuned.

[smc]: https://hackage.haskell.org/package/linear-smc-1.0.1

**achille** is an embedded Haskell DSL for building incremental static site generators.

It has a very simple abstraction for composable build rules, a straightforward
implementation and very few dependencies.

For more information on how **achille** works and how to use it, you can [read the
documentation](https://acatalepsie.fr/projects/achille/).
