---
title: Why
---

Here I'm gonna give a brief motivation for static-site generators in general,
and an explanation as to why I decided to make **achille**, and how it fares
with existing solutions.

## Static sites

Static sites are websites free of any dynamic content. They are usually stored
as a handful of HTML files, and therefore are *easy to deploy*, and *cheap to
host*. However, writing HTML files by hand can get tedious. Wouldn't it be
nice if we could *edit* the *content* of our websites using a friendly markup
language, and then *compile* this content to its final HTML form?
That's what *static site generators* (SSG) are for.

Static site generators have proven to be very useful tools for easily
generating static websites from neatly organised content files. Most of them
support using **markup languages** like markdown for writing content, and offer
**incremental compilation** so that updating a website stays **fast**,
regardless of its size. However, most SSGs are very opinionated about how you
should manage your content. As soon as your specific needs deviate slightly
from what your SSG supports, it becomes a lot more tedious.

This leads to many people writing their own personal static site generators
from scratch. This results in a completely personalised workflow, but without
good libraries it is a time-consuming endeavor, and incremental compilation is often
out of the equation as it is hard to get right.

This is where **achille** and [Hakyll][Hakyll] come in: they provide a *domain
specific language* embedded in Haskell to easily yet very precisely describe
how to build your site. Compile this description and **you get a full-fledged
static site generator with incremental compilation**, tailored specifically to
your needs.

[Hakyll]: https://jaspervdj.be/hakyll

### Why Hakyll is not enough

To provide incremental compilation, Hakyll relies on a global store, in which
all your *intermediate values* are stored. It is *your* responsibility to
populate it with *snapshots*. There are some severe limitations to this
approach:

- The store is **fundamentally untyped**, so **retrieving snapshots may fail at
  runtime** (well, *generation*-time) if you're not careful when writing your build rules. You may
  argue that's not very critical --- I think it shouldn't be possible in the
  first place. We are using a strongly typed language, so we shouldn't have
  to rely on flaky coercions at runtime to manipulate intermediate values.

- **Loading snapshots with glob patterns is awkward**. With Hakyll, *the*
  way to retrieve intermediate values is by querying the store,
  using glob patterns. This indirect way of managing values is very
  clumsy. In Haskell, the very purpose of variables is to store intermediate
  values, so we should only have to deal with plain old variables.

- **Dependencies are not explicit**. Because it relies on a global store for
  handling intermediate values, Hakyll has to make sure that the snaphots you
  want to load have been generated already. And because rules have no imposed
  order despite implicit inter-dependencies, Hakyll has to evaluate very
  carefully each rule, eventually pausing them to compute missing dependencies.
  This is very complex and quite frankly impressive, yet I believe we can strive
  for a simpler model of evaluation. If we used plain old variables to hold
  intermediate values, we simply would not be allowed to refer to an undefined
  variable.

There are other somewhat debatable design decisions:

- In Hakyll, every rule will produce an output file, and only one, if you're
  restricting yourself to the API they provide. I argue
  such a library should not care whether a rule produces any output on the
  filesystem. Its role is merely to know *if the rule must be executed*. Because of
  this requirement, producing multiple outputs from the same file is a tad
  cumbersome.
- Because Hakyll stores many content files directly in the store, the resulting
  cache is *huge*. This is unnecessary, the files are right here in the content
  directory.
- Hakyll uses a *lot* of abstractions --- `Compiler`, `Item`, `Rule`, `RuleSet`
  --- whose purpose is not obvious to a newcomer.
- It defines monads to allow the convenient `do` notation to be used, but
  disregards completely the very benefit of using monads --- it composes!

### Other tools

As always when thinking I am onto something, I jumped straight into code
and forgot to check whether there were alternatives. By fixating on Hakyll, I did not
realize many people have had the same comments about the shortcomings of Hakyll
and improved upon it. Therefore, it's only after building most of **achille**
in a week that I realized there were many
other similar tools available, namely: [rib][rib], [slick][slick], [Pencil][pencil] &
[Lykah][lykah].

[rib]:    https://rib.srid.ca/
[slick]:  https://hackage.haskell.org/package/slick
[pencil]: http://elbenshira.com/pencil/
[lykah]:  https://hackage.haskell.org/package/Lykah

Fortunately, I still believe **achille** is a significant improvement over these libraries.

- As far as I can tell, **pencil** does not provide incremental generation.
  It also relies on a global store, no longer untyped but very
  restrictive about what you can store. It implements its own templating language.
- Likewise, no incremental generation in **Lykah**.
  Reimplements its own HTML DSL rather than use *lucid*.
  Very opinionated, undocumented and unmaintained.
- **rib** and **slick** are the most feature-complete of the lot.
  They both provide a minimalist web-focused interface over the very powerful build system
  [Shake][Shake]. Shake is in itself a very complicated beast. The forward
  module that both libraries utilize is marked as *"experimental"* and depends
  on yet another tool to track file accesses: *fsatrace*.
  I'm not actually opposed to using Shake for dependency tracking at some point
  in **achille**, but ultimately I didn't find **rib** and **slick** to offer
  much more on top of it.

[Shake]: https://shakebuild.com/
