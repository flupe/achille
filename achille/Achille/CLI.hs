-- | CLI for achille recipes.
module Achille.CLI 
  ( achille
  , achilleWith
  )
  where

import Control.Monad (forM)
import Data.Binary (encode)
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Time (UTCTime(..))
import Options.Applicative
import System.FilePath ((</>))
import System.Directory (removePathForcibly)

import Achille.Cache
import Achille.Config (Config(..), defaultConfig, cacheFile)
import Achille.Diffable (unit)
import Achille.Dot (outputGraph)
import Achille.Task (Task, runTask)
import Achille.Recipe hiding (void)
import Achille.IO (AchilleIO(doesFileExist, readFileLazy, writeFileLazy))

import Data.Binary          qualified as Binary
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict      qualified as Map
import Data.Set             qualified as Set
import Achille.IO           qualified as AIO

import Achille.Core.Task (toProgram)
import Achille.Core.Recipe (Result(..), FileDeps(..))


-- TODO(flupe): make the CLI interace extensible
-- TODO(flupe): add ignore list


-- | CLI commands.
data AchilleCommand
    = Build Bool             -- ^ Build the site once.
    | Deploy                 -- ^ Deploy to the server.
    | Clean                  -- ^ Delete all artefacts.
    | Graph (Maybe FilePath) -- ^ Output graph of generator.
    deriving (Eq, Show)


-- | CLI parser.
achilleCLI :: Parser AchilleCommand
achilleCLI = subparser $
     command "build"  (info (Build <$> switch (long "force" <> short 'f')) 
                            (progDesc "Build the site"))
  <> command "deploy" (info (pure Deploy) 
                            (progDesc "Deploy site"))
  <> command "clean"  (info (pure Clean)
                            (progDesc "Delete all artefacts"))
  <> command "graph"  (info (Graph <$> optional (strOption (long "output" <> short 'o' <> metavar "FILENAME"))) 
                            (progDesc "Output graph of generator"))


-- NOTE(flupe): additional invariant: the list of file deps is already sorted
type GlobalCache = ([FilePath], Cache)

-- | Run a task in some context given a configuration.
runAchille 
  :: (Monad m, MonadFail m, AchilleIO m) 
  => Config
  -> Bool -- ^ Whether to force execution
  -> Task m a -> m ()
runAchille cfg@Config{..} force t = do
  -- 1. try to retrieve cache
  ((deps, cache) :: GlobalCache, lastTime) <- do
    hasCache <- doesFileExist cacheFile
    if hasCache && not force then do
      AIO.log "Loading cache…"
      (,) <$> (Binary.decode . LBS.fromStrict <$> AIO.readFile cacheFile)
          <*> AIO.getModificationTime cacheFile
    else pure (([], emptyCache), UTCTime (toEnum 0) 0)

  -- 2. retrieve mtime of all known dynamic dependencies
  updates :: Map FilePath UTCTime <-
    Map.fromAscList . catMaybes <$> forM deps \src -> do
      exists <- doesFileExist src
      if exists then Just . (src,) <$> AIO.getModificationTime src
                else pure Nothing

  -- 3. create initial context
  let ctx :: Context = Context
        { lastTime     = lastTime
        , currentDir   = ""
        , inputRoot    = contentDir
        , outputRoot   = outputDir
        , sitePrefix   = sitePrefix
        , updatedFiles = updates
        }

  -- 4. run task in context using cache
  Result _ (Deps newDeps) cache' <- runTask t ctx cache

  AIO.log "Reported dynamic dependencies: "
  AIO.log $ show newDeps

  -- 5. write new cache to file
  AIO.log "Saving cache…"
  AIO.writeFileLazy cacheFile $ Binary.encode ((Set.toList newDeps, cache') :: GlobalCache)

  AIO.log "All done!"

-- | Top-level runner for achille tasks. Provides a CLI with several commands.
achille :: Task IO a -> IO ()
achille = achilleWith defaultConfig
{-# INLINE achille #-}


-- | Top-level runner and CLI for achille programs, using a custom config.
achilleWith :: Config -> Task IO a -> IO ()
achilleWith cfg@Config{..} t = customExecParser p opts >>= \case
  Deploy       -> putStrLn "Deploying website..."
  Clean        -> putStrLn "Deleting all artefacts"
                  *> removePathForcibly cacheFile
                  *> removePathForcibly outputDir
  Build force  -> void $ runAchille cfg force t
  Graph output -> outputGraph output (toProgram t)
  where
    opts = info (achilleCLI <**> helper) $ fullDesc <> header description
    p    = prefs showHelpOnEmpty
{-# INLINE achilleWith #-}
