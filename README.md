## achille

achille is an Haskell library for building static site generators.
It is very inspired by [Hakyll], but much smaller in scope.

[Hakyll]: https://jaspervdj.be/hakyll/

- It makes no assumption about how you collect your sources,
  so you are free to retrieve data however you please.
- Intermediate values are made explicit by the user,
  which allows fine-tuned incremental builds.
- Because achille only cares about intermediate values,
  producing multiple versions of a file is in turn much easier.

achille provides many utilities for common use cases.
It optionally comes with recipes for converting documents using pandoc.

Additionally our `Task` description for constructing intermediate values is *a real monad*™, so we're not abusing the `do` notation!

