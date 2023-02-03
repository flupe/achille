---
title: achille
template: index
description: |
  achille is a Haskell EDSL for writing incremental static site generators
---

<header class="hero">
**achille** [aʃil] is a Haskell <abbr title="Embedded Domain-Specific Language">EDSL</abbr>
for writing *incremental static site generators*. It enables you to write
succinct, idiomatic Haskell code to describe the build process of your very own
website. Once compiled, this code yields a *fast*, *incremental* and *parallel*
custom site generator. Some kind of static site generator *generator*.
</header>

Features:

- A **tiny** library with a straightforward implementation and very **few dependencies**.
- **Extensible**. Custom build transformations can be implemented and provided
  by additional libraries. You pick and choose what you want!
- **Incremental** and **parallel**\* generators, without any annotation required from the user.
- **Visualization**. Your generator can dump a Graphviz representation of its
  static build graph --- and without having to run it --- if you ask for it.
- **Dynamic dependencies** are kept track of during execution, 
  meaning files changing inbetween runs will *always* trigger tasks
  appropriately.
- **Failure recovery**. Since the dependency graph of your build tasks is known at build time,
  if some of them fail during generation, it will always be able to complete
  all other tasks that are independent.

*Wishful thinking, on the roadmap but not (yet) implemented:*  
*(Though I think I have a solution for each of them in my head)*

- **Self-tracking**.

---

- Read the [user documentation](/docs.html) to get a better introduction to
  achille and how to use it.
- Feel free to send a mail to the public mailing list [~flupe/achille@lists.sr.ht][list] for troubleshooting,
    general questions or suggestions!

[list]: mailto:~flupe/achille@lists.sr.ht
