{-# LANGUAGE OverloadedStrings #-}
-- | CLI for achille recipes.
module Achille.CLI
  ( achille
  , achilleWith
  )
  where

import Control.Monad (forM, when)
import Data.Binary (encode)
import Data.Functor (void)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Time (UTCTime(..))
import Numeric (showFFloat)
import Options.Applicative
import System.Directory (removePathForcibly)
import System.Clock as Clock
import System.IO
import UnliftIO (MonadUnliftIO)

import Achille.Cache
import Achille.Config (Config(..), defaultConfig, cacheFile)
import Achille.Diffable (unit)
import Achille.Dot (outputGraph)
import Achille.Path
import Achille.Task (Task, runTask)
import Achille.Recipe hiding (void)
import Achille.IO (AchilleIO(doesFileExist, readFileLazy, writeFileLazy))

import Data.Binary          qualified as Binary
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict      qualified as Map
import Data.Set             qualified as Set
import Achille.IO           qualified as AIO

import Achille.Core.Task (toProgram)
import Achille.Context (Context(..))
import Achille.DynDeps
import Achille.Task.Prim
import Achille.Core.Recipe



-- TODO(flupe): make the CLI interace extensible
-- TODO(flupe): add ignore list

type Verbose    = Bool
type ForceBuild = Bool
type InColor    = Bool

-- | CLI commands.
data AchilleCommand
    = Build ForceBuild Verbose -- ^ Build the site once.
    | Deploy                   -- ^ Deploy to the server.
    | Clean                    -- ^ Delete all artefacts.
    | Graph (Maybe FilePath)   -- ^ Output graph of generator.
    deriving (Eq, Show)


-- | CLI parser.
achilleCLI :: Parser AchilleCommand
achilleCLI = subparser $
     command "build"  (info (Build <$> switch (long "force" <> short 'f')
                                   <*> switch (long "verbose" <> short 'v'))
                            (progDesc "Build the site"))
  <> command "deploy" (info (pure Deploy) 
                            (progDesc "Deploy site"))
  <> command "clean"  (info (pure Clean)
                            (progDesc "Delete all artefacts"))
  <> command "graph"  (info (Graph <$> optional (strOption (long "output" <> short 'o' <> metavar "FILENAME"))) 
                            (progDesc "Output graph of generator"))


-- NOTE(flupe): additional invariant: the list of file deps is already sorted
type GlobalCache = (DynDeps, Cache)

-- | Run a task in some context given a configuration.
runAchille
  :: (Monad m, MonadFail m, MonadUnliftIO m, AchilleIO m)
  => Config
  -> ForceBuild -- ^ Whether to force execution
  -> Verbose    -- ^ Whether to display debug logs
  -> InColor
  -> Task m a -> m ()
runAchille cfg@Config{..} force verbose colorful t = do
  -- 1. try to retrieve cache
  ((deps, cache), hasCache, lastTime) :: (GlobalCache, Bool, lastTime) <- do
    hasCache <- doesFileExist cacheFile
    if hasCache && not force then do
      when verbose $ AIO.log "Loading cache…"
      (,,) <$> (Binary.decode . LBS.fromStrict <$> AIO.readFile cacheFile)
           <*> pure True
           <*> AIO.getModificationTime cacheFile
    else pure ((mempty, emptyCache), False, UTCTime (toEnum 0) 0)

  -- 2. retrieve mtime of all known dynamic dependencies
  updates :: Map Path UTCTime <- do
    globFiles <- nub . concat <$> forM (getGlobDeps deps) (AIO.glob contentDir)
    globTimes <- Map.fromList <$> forM globFiles \src -> (src,) <$> AIO.getModificationTime src
    specTimes <- Map.fromAscList . catMaybes <$>
      forM (Set.toAscList $ getFileDeps deps) \src -> do
        exists <- (not (Map.member src globTimes) ||) <$> doesFileExist src
        if exists then Just . (src,) <$> AIO.getModificationTime src
                  else pure Nothing

    pure (globTimes <> specTimes)

  -- 3. create initial context
  let ctx :: Context = Context
        { lastTime     = lastTime
        , currentDir   = "."
        , updatedFiles = updates
        , cleanBuild   = not hasCache
        , siteConfig   = cfg
        , verbose      = verbose
        , colorful     = colorful
        }

  -- 4. run task in context using cache
  (_, cache', deps) <- runTask t ctx cache

  -- 5. write new cache to file
  when verbose $ AIO.log "Saving cache…"
  AIO.writeFileLazy cacheFile $ Binary.encode ((deps, cache') :: GlobalCache)


-- | Top-level runner for achille tasks. Provides a CLI with several commands.
achille :: Task IO a -> IO ()
achille = achilleWith defaultConfig
{-# INLINE achille #-}


-- | Top-level runner and CLI for achille programs, using a custom config.
achilleWith :: Config -> Task IO a -> IO ()
achilleWith cfg@Config{..} t = customExecParser p opts >>= \case
  Deploy       -> putStrLn "Deploying website..."
  Clean        -> putStrLn "Deleting all artefacts"
                  *> removePathForcibly (toFilePath cacheFile)
                  *> removePathForcibly (toFilePath outputDir)
  Build force verbose -> do
    colorful <- hIsTerminalDevice stdout
    start <- Clock.getTime Monotonic
    ()    <- runAchille cfg force verbose colorful t
    stop  <- Clock.getTime Monotonic
    putStrLn $ "All done! (" <> show (Duration (toNanoSecs $ diffTimeSpec stop start)) <> ")"
  Graph output -> outputGraph output (toProgram t)
  where
    opts = info (achilleCLI <**> helper) $ fullDesc <> header description
    p    = prefs showHelpOnEmpty
{-# INLINE achilleWith #-}


-- | A duration in picoseconds.
newtype Duration = Duration Integer

instance Show Duration where
  show (Duration d) = stab (d * 10) ["ns", "μs", "ms", "s"]
    where
      stab :: Integer -> [String] -> String
      stab x (unit:us@(_:_)) | x >= 10000 = stab (x `div` 1000) us
      stab x (unit:_) = showFFloat (Just 1) (fromIntegral x / 10) (" " <> unit)
