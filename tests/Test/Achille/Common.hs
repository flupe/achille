{-# LANGUAGE OverloadedStrings #-}
module Test.Achille.Common where

import Data.Map.Strict qualified as Map

import Achille.Path
import Achille.Config (defaultConfig, Config(outputDir))
import Achille.Context (Context(..))

import Test.Achille.FakeIO

-- | Base FS used for tests.
baseFS :: FileSystem
baseFS = Map.fromList
  [ ("content" </> "fichier.txt",         File defMTime "helloworld")
  , ("content" </> "post.md",             File defMTime "<em>hello</em>")
  , ("content" </> "other-post.md",       File defMTime "<strong>hello</strong>")
  , ("content" </> "dir1" </> "index.md", File defMTime "somecontent")
  , ("content" </> "dir1" </> "meta.md",  File defMTime "metadata of dir1")
  , ("content" </> "dir2" </> "index.md", File defMTime "some content again")
  , ("content" </> "dir2" </> "meta.md",  File defMTime "metadata of dir2")
  ]

-- | Base context used to run tasks.
baseCtx :: Context
baseCtx = Context
  { lastTime     = defMTime
  , updatedFiles = Map.empty
  , currentDir   = "."
  , cleanBuild   = True
  , siteConfig = defaultConfig { outputDir = "output" }
  , verbose = False
  , colorful = False
  }
