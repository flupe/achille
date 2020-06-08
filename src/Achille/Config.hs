module Achille.Config
    ( Config (..)
    , def
    ) where


import System.FilePath (FilePath)
import Data.Default    (Default, def)


data Config = Config
    { contentDir :: FilePath
    , outputDir  :: FilePath
    , cacheFile  :: FilePath
    , deployCmd  :: Maybe String
    }


instance Default Config where
    def = Config
        { contentDir = "content"
        , outputDir  = "_site"
        , cacheFile  = ".cache"
        , deployCmd  = Nothing
        }
