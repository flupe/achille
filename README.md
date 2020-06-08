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

-----

Here is an example of how to use this library. This produces a CLI static site generator with incremental builds.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Data.Functor ((<&>))
import System.FilePath ((-<.>))
import Achille
import Templates

main :: IO ()
main = achille do
    match_ "assets/theme.css" copy
    match_ "./quid.rst" $ compilePandoc <&> outer >>= saveTo (-<.> "html")

    pictures <- match "visual/*/*" do
        copy
        readImage
            <&> downscaleToFit (FitWidth 740)
            >>= saveThumbnailTo (-<.> "thumb.png")
            <&> timestampedWith (timestamp . thumbPath)

    with pictures $ match_ "./visual.rst" do
        txt    <- compilePandoc
        write "visual.html" $ renderVisual txt (recentFirst pictures)

    posts <- match "posts/*" do
        src <- copy
        compilePandoc
            <&> renderPost src
            >>= saveTo (-<.> "html")

    with posts $ match_ "./index.rst" do
        compilePandoc
            <&> renderIndex posts
            >>= saveTo (-<.> "html")
```
