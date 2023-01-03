---
title: Constructing build tasks
---

At the heart of **achille** is the *task* abstraction `Task m a`.
In order to describe how to generate your website, you will have to construct your
own specific build task.

`Task m a` represents a build task that will compute a value of type `a`, by
doing computations in `m`. To keep things simple, let's assume that `m` is `IO`
for the rest of this presentation.

Once you've come up with your own task `task :: Task IO ()`, you can retrieve a
CLI for your generator using `achille`:

```haskell
achille :: Task IO () -> IO ()
```

---

## Basic operations

`Task IO` is both a `Functor` and an `Applicative`, which should already hint at
some operations you get for free.

Additionally, **achille** exports the following combinators:

```haskell
(>>)  :: Task IO a -> Task IO b -> Task IO b
(>>=) :: Task IO a -> (Task IO a -> Task IO b) -> Task IO b
```

They are intended to be used with the `QualifiedDo` language extension (or
`RebindableSyntax`), and provide a convenient syntax for *putting tasks in
parallel* and *assigning intermediate results to variables*.

```haskell
import Achille as A

task1   :: Task IO Int
task2   :: Task IO Text
process :: Task IO Text -> Text IO ()

main :: IO ()
main = achille A.do
  task1
  txt <- task2
  process txt
```

### Matching files

**achille** provides two utilities to run tasks on all files matching a specific
*glob pattern*.

```haskell
match_ :: Pattern -> (Task IO FilePath -> Task IO a) -> Task IO ()
match  :: (Eq a, Binary a) 
       => Pattern -> (Task IO FilePath -> Task IO a) -> Task IO [a]
```

`match_` and `match` will execute the function on every filepath matching the
pattern. The only difference between the two is whether to discard the outputs
or not.

- `match_` discards the outputs of every execution of the function.
- `match` gathers all outputs and returns a list of them.

Because **achille** produces *incremental* site generators, it is *not* going to
run the function on every file matching the pattern *for every execution*.
Instead, it will store the result in its *cache* and use this value for as long
as the file associated with it *hasn't* changed. Hence the `Binary a`
constraint.

You may wonder whether `match` will be able to detect that results of tasks
assigned to variables *outside of `match`*, but used *inside the function* to
run on every file, may have changed, and re-trigger execution if such is the
case. The answer is **yes**! If any of the *external* variables used inside
`match` have changed since the last run, `match` will re-compute everything.

Consider the following example:

```haskell
import Achille as A
import Achille.Pandoc (processPandoc)
import Achille.Stache (loadTemplate)

main :: IO ()
main = achille A.do
  tpage <- loadTemplate "templates/page.mustache"

  match_ "*.md" \src ->
    processPandoc src
      & applyTemplate tpage
      & write (src -<.> "html")
```

- On the initial run, **achille** will read and cache the *mustache template*
  `page.mustache`, then it will read every markdown file, apply the template and
  write the corresponding HTML files.
- When you immediately run it again, *nothing happens*, because *nothing has
  changed*.
- If you change *one* markdown file, and run your generator, *only this file*
  gets processed again.
- But if you change the template file `page.mustache`, *every file* matched by
  `match` will be processed again, with the new template.

### Writing to the filesystem
