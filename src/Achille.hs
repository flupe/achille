module Achille
    ( module Achille.Config
    , module Achille.Timestamped
    , module Achille.Recipe
    , module Achille.Task
    , module Achille.Run

    , achille
    ) where


import Control.Monad        (void)
import System.Directory     (removePathForcibly)
import System.Process       (callCommand)
import Options.Applicative

import Achille.Config
import Achille.Timestamped
import Achille.Recipe
import Achille.Task
import Achille.Run


data Command
    = Build   -- ^ Build the site once
    | Deploy  -- ^ Deploy to the server
    | Clean   -- ^ Delete all artefacts
    deriving (Eq, Show)


cli :: Parser Command
cli = subparser $
      command "build"  (info (pure Build)  (progDesc "Build the site once" ))
   <> command "deploy" (info (pure Deploy) (progDesc "Server go brrr"      ))
   <> command "clean"  (info (pure Clean)  (progDesc "Delete all artefacts"))


-- | CLI interface for running a task
achille :: Task a -> IO ()
achille task = customExecParser p opts >>= \case
    Deploy -> callCommand deployCmd
    Clean  -> removePathForcibly outputDir
           >> removePathForcibly cacheFile
    Build  -> void $ run task
    where
        opts = info (cli <**> helper) $ fullDesc <> header desc
        p    = prefs showHelpOnEmpty
        desc = "A static site generator for fun and profit"
