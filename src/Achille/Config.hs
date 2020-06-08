module Achille.Config
    ( contentDir
    , outputDir
    , cacheFile
    , deployCmd
    ) where

import System.FilePath (FilePath)

contentDir :: FilePath
contentDir = "content"

outputDir :: FilePath
outputDir = "_site"

cacheFile :: FilePath
cacheFile = ".cache"

deployCmd :: String
deployCmd = "rsync -avzzr --chmod=755 _site/ flupe@duckduck:/var/www/acatalepsie"
