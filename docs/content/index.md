---
title: achille
---

**achille** is a Haskell EDSL for writing *incremental static site generators*.
It enables you to write succinct, idiomatic Haskell code to describe the build process
of your very own website. Once compiled, this code yields a *fast*, *incremental*
and *parallel* custom site generator.


The following code:

```haskell
import Achille as A

main = achille A.do
  match_ "assets/*" copy
  templates <- loadTemplates "templates"
  match_ "posts/*.md" \src -> A.do
    meta :*: content <- processPandoc src
    applyTemplate (templates ! "post") (Post <$> meta <*> content)
      & write (src -<.> "index.html")
```

Compiles into a generator with a CLI:

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
