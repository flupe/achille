-- | Exports a datatype for the top-level achille config.
module Achille.Config
    ( Config (..)
    , def
    ) where


import System.FilePath      (FilePath)
import System.FilePath.Glob (Pattern)
import Data.Default         (Default, def)


-- | achille configuration datatype.
data Config = Config
    { contentDir :: FilePath      -- ^ Root of the source directory.
                                  --   Defaults to @"content"@.
    , outputDir  :: FilePath      -- ^ Root of the output directory.
                                  --   Defaults to @"_site"@.
    , cacheFile  :: FilePath      -- ^ Path where the cache is stored.
                                  --   Defaults to @".cache"@.
    , deployCmd  :: Maybe String  -- ^ Command to run for deploying the result.
    , ignore     :: [Pattern]
    }


instance Default Config where
    def = Config
        { contentDir = "content"
        , outputDir  = "_site"
        , cacheFile  = ".cache"
        , deployCmd  = Nothing
        , ignore     = []
        }
