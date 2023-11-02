{-# LANGUAGE OverloadedStrings #-}
module Test.Achille.Common where

import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Time (UTCTime(..))

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map

import Achille.Path
import Achille.Config (Config(..), defaultConfig)
import Achille.Context (Context(..))

import System.FilePath.Glob qualified as Glob


defMTime :: UTCTime
defMTime = UTCTime (toEnum 0) 0


data File = File
  { mtime    :: UTCTime
  , contents :: BS.ByteString
  }

type FileSystem = Map Path File

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

baseCfg :: Config
baseCfg = defaultConfig { outputDir = "output" }

-- | Base context used to run tasks.
baseCtx :: Context
baseCtx = Context
  { lastTime     = defMTime
  , currentTime  = defMTime
  , updatedFiles = Map.empty
  , currentDir   = ""
  , cleanBuild   = True
  , siteConfig   = baseCfg
  , verbose      = False
  , colorful     = False
  }

getMTime :: Path -> FileSystem -> UTCTime
getMTime src = maybe defMTime mtime . Map.lookup src

getBS :: Path -> FileSystem -> BS.ByteString
getBS src = maybe BS.empty contents . Map.lookup src

-- | Poor man's glob. Inefficient, but that's not the point.
globFS :: Path -> Glob.Pattern -> FileSystem -> [Path]
globFS root pat fs = do
    map (makeRelative (contentDir baseCfg)) (Map.keys fs)
  & filter (Glob.matchWith Glob.matchDefault pat . toFilePath)
  & map (contentDir baseCfg </>)
