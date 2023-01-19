{-# LANGUAGE DerivingStrategies #-}
module Achille.Result
  ( Result
  , runResult
  , setDeps
  , getContext
  , getConfig
  , lift
  , contentDir
  , outputDir
  , updatedFiles
  , lastTime
  ) where

import Data.Map.Strict (Map)
import Data.Time (UTCTime)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Reader.Class  (MonadReader, reader, ask, asks)
import Control.Monad.Writer.Class  (MonadWriter, writer, tell)
import Control.Monad.Writer.Strict (WriterT, runWriterT)
import Control.Monad.Trans.Reader  (ReaderT, runReaderT)

import Data.Map.Strict qualified as Map

import Achille.Config (Config)
import Achille.Context (Context)
import Achille.DynDeps (DynDeps, dependsOnFile, dependsOnPattern)
import Achille.Path (Path)
import Achille.IO as AIO

import Achille.Config qualified as Cfg
import Achille.Context qualified as Ctx


-- NOTE(flupe): Result is not a good name, as it actually represents an effectful computation in context.

newtype Result m a = Result (ReaderT Context (WriterT DynDeps m) a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader Context, MonadWriter DynDeps)
  -- TODO(flupe): MonadReader Config Result

runResult :: Result m a -> Context -> m (a, DynDeps)
runResult (Result r) = runWriterT . runReaderT r

instance MonadTrans Result where
  {-# INLINE lift #-}
  lift = Result . lift . lift

-- NOTE(flupe): maybe for AchilleIO (Result m) we actually want to do
--              smart path transformation?

instance (Monad m, AchilleIO m) => AchilleIO (Result m) where
  getModificationTime path =
    reader ((Map.!? path) . Ctx.updatedFiles) >>= \case
      Just mtime -> pure mtime
      Nothing    -> lift $ AIO.getModificationTime path
    >>= writer . (, dependsOnFile path)
  readFile path     = lift (AIO.readFile path)     <* tell (dependsOnFile path)
  readFileLazy path = lift (AIO.readFileLazy path) <* tell (dependsOnFile path)
  copyFile from to  = lift (AIO.copyFile from to)  <* tell (dependsOnFile from)
  writeFile path  bs     = lift $ AIO.writeFile path bs
  writeFileLazy path lbs = lift $ AIO.writeFileLazy path lbs
  doesFileExist path = lift (AIO.doesFileExist path) <* tell (dependsOnFile path)
  doesDirExist path = lift (AIO.doesDirExist path) -- TODO(flupe): check semantics of mtime of dir
  listDir = lift . AIO.listDir
  callCommand = lift . AIO.callCommand
  log = lift . AIO.log
  readCommand cmd args = lift (AIO.readCommand cmd args)
  glob root pat = lift (AIO.glob root pat) <* tell (dependsOnPattern pat)


-- TODO(flupe): error recovery with non-failing fail
instance MonadFail m => MonadFail (Result m) where fail = lift . fail

-- Some context projections to avoid having to do it manually each time
contentDir :: Monad m => Result m Path
contentDir = reader (Cfg.contentDir . Ctx.siteConfig)

outputDir :: Monad m => Result m Path
outputDir = reader (Cfg.outputDir . Ctx.siteConfig)

updatedFiles :: Monad m => Result m (Map Path UTCTime)
updatedFiles = reader Ctx.updatedFiles

lastTime :: Monad m => Result m UTCTime
lastTime = reader Ctx.lastTime

getContext :: Monad m => Result m Context
getContext = ask

getConfig :: Monad m => Result m Config
getConfig = asks Ctx.siteConfig

setDeps :: Monad m => DynDeps -> Result m ()
setDeps = tell
