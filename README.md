## achille

achille is an Haskell library for building static site generators.
It is very inspired by [Hakyll], but much smaller in scope.

[Hakyll]: https://jaspervdj.be/hakyll/

- It makes no assumption about how you collect your data,
  so you are free to retrieve resources however you please.
- Intermediate values and dependencies are handled explicitly by the user,
  which allows for efficient fine-tuned incremental builds and a tiny persistent cache.
- Because achille only cares about your intermediate values,
  producing multiple versions of a file is in turn much easier.

While trying to stay minimal, achille still provides many utilities for common use cases.
It optionally comes with recipes for converting documents using pandoc,
and recipes for generating thumbnail images.

And our `Task` description for constructing intermediate values is *a real monad*™, so we're not abusing the `do` notation!
