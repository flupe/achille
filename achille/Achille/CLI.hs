{-# LANGUAGE Rank2Types, ViewPatterns, RecordWildCards, NamedFieldPuns #-}

-- | CLI for achille recipes.
module Achille.CLI 
  ( achille
  , achilleWith
  )
  where

import Data.Binary (encode)
import Data.Functor (void)
import Data.Time (UTCTime(..))
import Options.Applicative

import Achille.Cache
import Achille.Config (Config(..), defaultConfig, cacheFile)
import Achille.Diffable (unit)
import Achille.Task (Task, toTask, runTask)
import Achille.Recipe hiding (void)
import Achille.Syntax (Program)
import Achille.IO (AchilleIO(doesFileExist, readFileLazy, writeFileLazy))

import Data.Binary          qualified as Binary
import Achille.IO           qualified as AIO
import Data.ByteString.Lazy qualified as LBS


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
     command "build" (info (pure Build) (progDesc "Build the site"))
  <> command "deploy" (info (pure Deploy) (progDesc "Deploy site"))
  <> command "clean" (info (pure Clean) (progDesc "Delete all artefacts"))
  <> command "graph" (info (pure Graph) (progDesc "Output graph of generator"))


-- | Run a task in some context given a configuration.
runAchille :: (Monad m, MonadFail m, AchilleIO m) => Config -> Task m a -> m ()
runAchille cfg@Config{..} t = do
  -- 1. try to retrieve cache
  (cache, lastTime) <- do
    hasCache <- doesFileExist cacheFile
    if hasCache then
      (,) <$> (Binary.decode . LBS.fromStrict <$> AIO.readFile cacheFile)
          <*> AIO.getModificationTime cacheFile
    else pure (emptyCache, defaultTime)

  -- 2. create initial context
  let ctx :: Context = Context
        { lastTime   = lastTime
        , currentDir = ""
        , inputRoot  = contentDir
        , outputRoot = outputDir
        }

  -- 3. run task in context using cache
  (_, cache') <- runTask ctx cache t

  -- 4. write new cache to file
  AIO.writeFileLazy cacheFile $ Binary.encode cache'

  where defaultTime :: UTCTime
        defaultTime = UTCTime (toEnum 0) 0


-- | Top-level runner for achille tasks. Provides a CLI with several commands.
achille :: Program IO a -> IO ()
achille = achilleWith defaultConfig
{-# INLINE achille #-}


-- | Top-level runner and CLI for achille programs, using a custom config.
achilleWith :: Config -> Program IO a -> IO ()
achilleWith cfg@Config{description} (toTask -> t) = customExecParser p opts >>= \case
  Deploy -> putStrLn "Deploying website..."
  Clean  -> putStrLn "Deleting all artefacts"
  Build  -> void $ runAchille cfg t
  Graph  -> print t
  where
    opts = info (achilleCLI <**> helper) $ fullDesc <> header description
    p    = prefs showHelpOnEmpty
{-# INLINE achilleWith #-}
