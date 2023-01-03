---
title: achille
template: index
description: |
  achille is a Haskell EDSL for writing incremental static site generators
---

**achille** [aʃil] is a Haskell <abbr title="Embedded Domain-Specific Language">EDSL</abbr>
for writing *incremental static site generators*. It enables you to write
succinct, idiomatic Haskell code to describe the build process of your very own
website. Once compiled, this code yields a *fast*, *incremental* and *parallel*
custom site generator.

Features:

- A **tiny** library with a straightforward implementation and very **few dependencies**.
- **Extensible**. Custom build transformations can be implemented and provided
  by additional libraries. You pick and choose what you want!
- **Incremental** and **parallel** generators, without any annotation required from the user.

*Wishful thinking, on the roadmap but not (yet) implemented:*

- **Visualization**. Your generator can dump a Graphviz representation of its
  build graph, if you ask for it.
- **Failure recovery**. Since the dependency graph of your build tasks is known at build time,
  if some of them fail during generation, it will always be able to complete
  all other tasks that are independent.
- **Self-tracking**.

```bash
$ site
My very own static-site generator!

Usage: site COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Build the site once
  deploy                   Server go brrr
  clean                    Delete all artefacts
```

---

- Read the [user documentation](/docs.html) to get a better introduction to
  achille and how to use it.
- Feel free to send a mail to the public mailing list [~flupe/achille@lists.sr.ht][list] for troubleshooting,
    general questions or suggestions!

[list]: mailto:~flupe/achille@lists.sr.ht
