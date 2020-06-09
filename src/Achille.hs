module Achille
    ( module Achille.Config
    , module Achille.Timestamped
    , module Achille.Thumbnail
    , module Achille.Recipe
    , module Achille.Task
    , module Achille.Run

    , achille
    ) where


import Control.Monad         (void, mapM_)
import System.Directory      (removePathForcibly)
import System.Process        (callCommand)
import System.FilePath.Glob  (compile)
import Options.Applicative

import Achille.Config
import Achille.Timestamped
import Achille.Thumbnail
import Achille.Recipe hiding (Context)
import Achille.Task
import Achille.Run    hiding (Context)


data Command
    = Build [String]  -- ^ Build the site once
    | Deploy          -- ^ Deploy to the server
    | Clean           -- ^ Delete all artefacts
    deriving (Eq, Show)


cli :: Parser Command
cli = subparser $
      command "build"  (info (Build <$> many (argument str (metavar "FILES")))  (progDesc "Build the site once" ))
   <> command "deploy" (info (pure Deploy) (progDesc "Server go brrr"      ))
   <> command "clean"  (info (pure Clean)  (progDesc "Delete all artefacts"))


-- | CLI interface for running a task
achille :: Task a -> IO ()
achille = achilleWith def


-- | CLI interface for running a task using given options
achilleWith :: Config -> Task a -> IO ()
achilleWith config task = customExecParser p opts >>= \case
    Deploy -> mapM_ callCommand  (deployCmd config)
    Clean  -> removePathForcibly (outputDir config)
           >> removePathForcibly (cacheFile config)
    Build paths -> void $ run (map compile paths) config task
    where
        opts = info (cli <**> helper) $ fullDesc <> header desc
        p    = prefs showHelpOnEmpty
        desc = "A static site generator for fun and profit"
