## achille

[![View on Hackage][hackage-shields]][hackage]
[![Github Workflow][ci-shields]][ci]
[![Project license][license-shields]][license]

[hackage]: https://hackage.haskell.org/package/achille
[hackage-shields]: https://img.shields.io/hackage/v/achille?style=flat-square

[ci]: https://github.com/flupe/achille/actions/workflows/haskell.yml
[ci-shields]: https://img.shields.io/github/actions/workflow/status/flupe/achille/haskell.yml?label=build%20%26%20test&style=flat-square

[license]: https://github.com/flupe/achille/blob/master/LICENSE
[license-shields]: https://img.shields.io/github/license/flupe/achille?style=flat-square

> **Note**
>
> The only version available on Hackage is largely outdated and *shouldn't be used*.
> This repository contains a WIP rewrite, that should make achille *way* easier to use
> and even more powerful.

[smc]: https://hackage.haskell.org/package/linear-smc-1.0.1

**achille** is an embedded Haskell DSL for building incremental static site generators.

It has a very simple abstraction for composable build rules, a straightforward
implementation and few dependencies.

For troubleshooting, general questions or suggestions, feel free to send a mail
to the public mailing list [~flupe/achille@lists.sr.ht][list]. Archives are
available [here][archives].

[list]: mailto:~flupe/achille@lists.sr.ht
[archives]: https://lists.sr.ht/~flupe/achille

---

Roadmap to release `0.1.0`:

- [x] Bring back some unit testing infrastructure.
- [x] `Path`.
- [ ] `URL`.
- [ ] Full dynamic dependencies (i.e not only specific files, but also
      dependencies on glob patterns).
- [ ] Error-recovery.
- [ ] Parallelism/Concurrency for free.
- [ ] Fix implementation of achille-stache by taking the transitive closure of
      dependencies.
- [ ] More core recipies like `runCommand`.
- [ ] Better graph rendering (maybe stop using Graphviz).
- [ ] Proper logging with verbosity.
- [ ] Nicer Achille prelude.
- [ ] Self-tracking with merkle tree.
  - [ ] Hash pandoc options.
