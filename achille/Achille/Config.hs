{-# LANGUAGE OverloadedStrings #-}
-- | Exports a datatype for the top-level achille configuration
module Achille.Config
    ( Config (..)
    , defaultConfig
    ) where


import Achille.Path (Path)


data Config = Config
    { contentDir  :: Path -- ^ Root of the source directory.
                              --   Defaults to @"content"@.
    , outputDir   :: Path -- ^ Root of the output directory.
                              --   Defaults to @"_site"@.
    , cacheFile   :: Path -- ^ Path where the cache is stored.
                              --   Defaults to @".cache"@.
    , sitePrefix  :: FilePath
    , description :: String
    }


defaultConfig :: Config
defaultConfig = Config
  { contentDir  = "content"
  , outputDir   = "_site"
  , cacheFile   = ".cache"
  , sitePrefix  = ""
  , description = "My very own static-site generator :)"
  }
