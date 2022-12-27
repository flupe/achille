-- | Exports a datatype for the top-level achille configuration
module Achille.Config
    ( Config (..)
    , defaultConfig
    ) where


import System.FilePath (FilePath)


data Config = Config
    { contentDir  :: FilePath      -- ^ Root of the source directory.
                                  --   Defaults to @"content"@.
    , outputDir   :: FilePath      -- ^ Root of the output directory.
                                  --   Defaults to @"_site"@.
    , cacheFile   :: FilePath      -- ^ Path where the cache is stored.
                                  --   Defaults to @".cache"@.
    , description :: String
    }


defaultConfig :: Config
defaultConfig = Config
  { contentDir  = "content"
  , outputDir   = "_site"
  , cacheFile   = ".cache"
  , description = "My very own static-site generator :)"
  }
