-- | Top-level module for achille, providing the CLI and task runner.
module Achille
    ( module Achille.Config
    , module Achille.Timestamped
    , module Achille.Task
    , module Achille.Task.Cached
    , AchilleCommand
    , achilleCLI
    , achille
    , achilleWith
    ) where


import Control.Monad           (void, mapM_)
import Control.Monad.IO.Class  (MonadIO)
import System.Directory        (removePathForcibly)
import System.FilePath.Glob    (compile)
import Options.Applicative

import qualified System.Process as Process

import Achille.Config
import Achille.Timestamped
import Achille.Task
import Achille.Task.Cached


-- | CLI commands.
data AchilleCommand
    = Build [String]  -- ^ Build the site once
    | Deploy          -- ^ Deploy to the server
    | Clean           -- ^ Delete all artefacts
    deriving (Eq, Show)


-- | CLI parser.
achilleCLI :: Parser AchilleCommand
achilleCLI = subparser $
      command "build"  (info (Build <$> many (argument str (metavar "FILES")))  (progDesc "Build the site once" ))
   <> command "deploy" (info (pure Deploy) (progDesc "Server go brrr"      ))
   <> command "clean"  (info (pure Clean)  (progDesc "Delete all artefacts"))


-- | Main entrypoint for achille. Provides a CLI for running a task.
achille :: Task IO a -> IO ()
achille = achilleWith def


-- | CLI for running a task using given options.
achilleWith :: Config -> Task IO a -> IO ()
achilleWith config task = customExecParser p opts >>= \case
    Deploy -> mapM_ Process.callCommand (deployCmd config)
    Clean  -> removePathForcibly (outputDir config)
           >> removePathForcibly (cacheFile config)
    Build paths -> void $ runTask (map compile paths) config task
    where
        opts = info (achilleCLI <**> helper) $ fullDesc <> header desc
        p    = prefs showHelpOnEmpty
        desc = "A static site generator for fun and profit"
