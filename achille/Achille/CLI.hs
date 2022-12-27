{-# LANGUAGE Rank2Types, ViewPatterns, RecordWildCards #-}

-- | CLI for achille recipes.
module Achille.CLI 
  ( achille
  , achilleWith
  )
  where

import Data.Binary (encode)
import Data.Functor (void)
import Options.Applicative

import Achille.Cache
import Achille.Config (Config(..), defaultConfig, cacheFile)
import Achille.Diffable (unit)
import Achille.Task (Task, toTask)
import Achille.Recipe
import Achille.Syntax (Program)
import Achille.IO (AchilleIO(doesFileExist, readFileLazy, writeFileLazy))


-- TODO(flupe): make the CLI interace extensible
-- TODO(flupe): add ignore list


-- | CLI commands.
data AchilleCommand
    = Build  -- ^ Build the site once.
    | Deploy -- ^ Deploy to the server.
    | Clean  -- ^ Delete all artefacts.
    | Graph  -- ^ Output graph of generator.
    deriving (Eq, Show)


-- | CLI parser.
achilleCLI :: Parser AchilleCommand
achilleCLI = subparser $
     command "build" (info (pure Build) (progDesc "Build the site once"))
  <> command "deploy" (info (pure Deploy) (progDesc "Deploy site"))
  <> command "clean" (info (pure Clean) (progDesc "Delete all artefacts"))
  <> command "graph" (info (pure Clean) (progDesc "Output graph of generator"))


-- | Run a task in some context given a configuration.
runAchille :: (Monad m, AchilleIO m) => Config -> Task m a -> m a
runAchille cfg t = undefined --do
--  cache <- do
--    hasCache <- doesFileExist $ cacheFile cfg
--    if hasCache then toCache <$> readFileLazy (cacheFile cfg) 
--                else pure emptyCache
--  ((v, _), cache') <- runRecipe t ctx cache unitV
--  cache' `seq` writeFileLazy (cacheFile cfg) $ encode cache'
--  -- TODO: ^ this doesn't look like a very nice way to do file I/O
--  --         investigate whether we should stop using lazy bytestrings or smthg
--  pure v


-- | Top-level runner for achille tasks. Provides a CLI with several commands.
achille :: Program IO a -> IO ()
achille = achilleWith defaultConfig
{-# INLINE achille #-}


-- | Top-level runner and CLI for achille programs, using a custom config.
achilleWith :: Config -> Program IO a -> IO ()
achilleWith cfg@Config{..} (toTask -> t) = customExecParser p opts >>= \case
  Deploy -> putStrLn "Deploying website..."
  Clean  -> putStrLn "Deleting all artefacts"
  Build  -> void $ runAchille cfg t
  where
    opts = info (achilleCLI <**> helper) $ fullDesc <> header description
    p    = prefs showHelpOnEmpty
{-# INLINE achilleWith #-}

