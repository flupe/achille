{-# LANGUAGE DerivingStrategies #-}
module Achille.Result where
--  ( Result
--  , runResult
--  , setDeps
--  , getContext
--  , getConfig
--  , lift
--  , contentDir
--  , outputDir
--  , updatedFiles
--  , lastTime
--  ) where

import Data.Map.Strict (Map)
import Data.Function ((&))
import Data.Functor.Compose
import Data.Time (UTCTime)
import Control.Applicative (liftA2)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.RWS.Strict
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class

import Data.Map.Strict qualified as Map

import Achille.Cache
import Achille.Config (Config)
import Achille.Context (Context)
import Achille.DynDeps (DynDeps, dependsOnFile, dependsOnPattern)
import Achille.Path (Path)
import Achille.IO as AIO

import Achille.Config qualified as Cfg
import Achille.Context qualified as Ctx


-- PrimTask m a ≃ Context -> Cache -> m (Maybe a, DynDeps, Cache)
newtype PrimTask m a = PrimTask
  { unPrimTask :: Compose (RWST Context DynDeps Cache m) Maybe a
  } deriving newtype (Functor, Applicative)

instance Monad m => Monad (PrimTask m) where
  PrimTask (Compose x) >>= f =
    PrimTask $ Compose $ x >>= \case
      Nothing -> pure Nothing
      Just x  -> getCompose (unPrimTask (f x))

instance Monad m => MonadReader Context (PrimTask m) where
  ask = PrimTask $ Compose $ Just <$> ask
  local f (PrimTask (Compose x)) = PrimTask (Compose (local f x))

instance Monad m => MonadWriter DynDeps (PrimTask m) where
  writer (x, w) = PrimTask $ Compose $ writer (Just x, w)
  tell deps = PrimTask (Compose (Just <$> tell deps))
  listen (PrimTask (Compose x)) =
    PrimTask $ Compose do
      (y, deps) <- listen x
      pure (liftA2 (,) y (pure deps))
  pass (PrimTask (Compose x)) =
    PrimTask $ Compose do
      (y, deps) <- listen x
      case y of
        Nothing -> pure Nothing
        Just (z, f) -> writer (Just z, f deps)

instance Monad m => MonadState Cache (PrimTask m) where
  get = PrimTask $ Compose $ Just <$> get
  put cache = PrimTask $ Compose $ Just <$> put cache

instance MonadTrans PrimTask where
  lift = PrimTask . Compose . lift . fmap Just

silentFail :: Monad m => PrimTask m a
silentFail = PrimTask (Compose (pure Nothing))

forward :: Monad m => Maybe a -> PrimTask m a
forward Nothing = silentFail
forward (Just x) = pure x

instance (Monad m, AchilleIO m) => MonadFail (PrimTask m) where
  fail s = lift (AIO.log s) *> PrimTask (Compose (pure Nothing))

runPrimTask :: PrimTask m a -> Context -> Cache -> m (Maybe a, Cache, DynDeps)
runPrimTask (PrimTask (Compose x)) = runRWST x

instance (Monad m, AchilleIO m) => AchilleIO (PrimTask m) where
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

-- NOTE(flupe): ^ maybe for AchilleIO (PrimTask m) we actually want to do
--                smart path transformation?

withCache :: Monad m => Cache -> PrimTask m a -> PrimTask m (Maybe a, Cache)
withCache lcache (PrimTask (Compose t)) = PrimTask $ Compose $ RWST \ctx cache -> do
  (x, lcache, deps) <- runRWST t ctx lcache
  pure (Just (x, lcache), cache, deps)

getCache :: Monad m => PrimTask m Cache
getCache = get

putCache :: Monad m => Cache -> PrimTask m ()
putCache = put

-- Some context projections to avoid having to do it manually each time
contentDir :: Monad m => PrimTask m Path
contentDir = reader (Cfg.contentDir . Ctx.siteConfig)

outputDir :: Monad m => PrimTask m Path
outputDir = reader (Cfg.outputDir . Ctx.siteConfig)

updatedFiles :: Monad m => PrimTask m (Map Path UTCTime)
updatedFiles = reader Ctx.updatedFiles

lastTime :: Monad m => PrimTask m UTCTime
lastTime = reader Ctx.lastTime

getContext :: Monad m => PrimTask m Context
getContext = ask

getConfig :: Monad m => PrimTask m Config
getConfig = asks Ctx.siteConfig

setDeps :: Monad m => DynDeps -> PrimTask m ()
setDeps = tell
