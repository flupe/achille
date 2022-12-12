-- | CLI for achille recipes.
module Achille.CLI where

import Data.Binary (encode)

import Achille.Cache
import Achille.Config (Config, defaultConfig, cacheFile)
import Achille.Diffable (unitV)
import Achille.Recipe
import Achille.IO     (AchilleIO(doesFileExist, readFileLazy, writeFileLazy))

-- | Run a task in some context given a configuration.
runTask :: (Monad m, AchilleIO m) => Config -> Context -> Task m a -> m a
runTask cfg ctx t = do
  cache <- do
    hasCache <- doesFileExist $ cacheFile cfg
    if hasCache then toCache <$> readFileLazy (cacheFile cfg) 
                else pure emptyCache
  ((v, _), cache') <- runRecipe t ctx cache unitV
  cache' `seq` writeFileLazy (cacheFile cfg) $ encode cache'
  -- TODO: ^ this doesn't look like a very nice way to do file I/O
  --         investigate whether we should stop using lazy bytestrings or smthg
  pure v

-- | Top-level runner for achille tasks. Will provide a CLI with several commands.
achille :: Task IO a -> IO a
achille = achilleWith defaultConfig

-- | Top-level runner for achille tasks, with a custom config.
achilleWith :: Config -> Task IO a -> IO a
achilleWith cfg t = runTask cfg Context t
