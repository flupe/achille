---
title: Making a blog from scratch
description: |
  Guide explaining how to write a static blog generator with achille
---

In this guide, we're gonna look into all the steps required to define a static
site generator for a little personal weblog.

## Content structure

Usually, a good way to reason about build rules is to first identify the kind of
content we want to manage, and how it's going to be organized.

For a little blog, the following structure isn't too surprising:

```
content/
├── index.md
├── about.md
├── posts
│   ├── first.md
│   └── second.md
├── assets
│   └── theme.css
└── templates
    ├── footer.mustache
    ├── header.mustache
    ├── index.mustache
    ├── page.mustache
    ├── postlist.mustache
    └── post.mustache
```

- *Pages* live at the root of the `content/` folder, and are written in Markdown.
- Blog *posts* are stored in `content/posts`, also written in Markdown.
- Static *assets* like CSS files are gathered in `content/assets`.
- Finally, we're gonna use Mustache templates to easily manage the HTML theme of
  our blog.

### Post metadata

Posts will have metadata specified in a YAML frontmatter at their very top.

```yaml
---
title: My blog post
date: 2023-01-03 08:32 Z
tags: [meta, haskell]
---
```

*Tags* are optional, but we will generate pages for every tag used in
blogposts, displaying all articles tagged under it.

---

## Getting started

### Setting up the project

```cabal
```

### Defining metadata containers

**achille-pandoc** provides utilities to read Markdown files and their
*frontmatter*, in such a way that the data we retrieve is well-formed and
well-typed. If a blog post has invalid metadata, you will get error messages
during generation. That's good! To benefit from this, you need to define
datatypes that encode the expected metadata information. Using `Generic` and
`aeson`, we can automatically derive JSON conversion.

```haskell
import Data.Text (Text)
import Data.Time (UTCTime)

data PostMeta = PostMeta
  { title :: Text
  , date  :: UTCTime
  , tags  :: Maybe [Text]
  } deriving (Generic, FromJSON, ToJSON)

data PageMeta = PageMeta
  { title :: Text
  } deriving (Generic, FromJSON, ToJSON)
```

### A first pass

```haskell
import Achille as A

main :: IO ()
main = achille A.do
  -- ...
```

#### Copying static assets

```haskell
match_ "assets/*" copy
```

#### Loading all templates

```haskell
templates :: Task IO (Map PName Template) <- loadTemplates "templates"
```

The type annotation is only here to show that `loadTemplates` will compute a
key-value store, mapping names to Mustache templates.

#### Rendering pages

```haskell
match_ "pages/*.md" \src -> A.do
  meta :*: content <- processPandocMeta src
  data <- PageData <$> meta <*> content
  applyTemplate (templates ! "page") data
    & write (src -<.> "html")
```

#### Rendering posts

```haskell
match_ "posts/*.md" \src -> A.do
  meta :*: content <- processPandocMeta src
  data <- PostData <$> meta <*> content
  applyTemplate (templates ! "post") data
    & write (src -<.> "html")
```
