{-# LANGUAGE DerivingStrategies #-}
module Achille.Task.Prim
  (  module Achille.Cache
  , module Achille.Config
  , module Achille.Context
  , PrimTask
  , runPrimTask
  , lift
  , halt
  , forward
  , setDeps
  , getContext
  , getConfig
  , withCache
  , fromCache
  , splitCache
  , joinCache
  , toCache
  , contentDir
  , outputDir
  , updatedFiles
  , lastTime
  ) where

import Data.Binary (Binary)
import Data.Map.Strict (Map)
import Data.Function ((&))
import Data.Time (UTCTime)
import Control.Applicative (liftA2)
import Control.Monad.RWS.Strict
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Class

import Achille.Cache (Cache)
import Achille.Config (Config)
import Achille.Context (Context)
import Achille.DynDeps (DynDeps, dependsOnFile, dependsOnPattern)
import Achille.Path (Path)
import Achille.IO as AIO

import Data.Map.Strict qualified as Map
import Achille.Cache   qualified as Cache
import Achille.Config  qualified as Cfg
import Achille.Context qualified as Ctx


-- PrimTask m a ≃ Context -> Cache -> m (Maybe a, DynDeps, Cache)
newtype PrimTask m a =
  PrimTask { unPrimTask :: MaybeT (RWST Context DynDeps Cache m) a }
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader Context, MonadState Cache, MonadWriter DynDeps)

instance MonadTrans PrimTask where
  lift = PrimTask . lift . lift

halt :: Monad m => PrimTask m a
halt = PrimTask $ MaybeT (pure Nothing)

forward :: Monad m => Maybe a -> PrimTask m a
forward Nothing = halt
forward (Just x) = pure x

instance (Monad m, AchilleIO m) => MonadFail (PrimTask m) where
  fail s = lift (AIO.debug s) *> halt

runPrimTask :: PrimTask m a -> Context -> Cache -> m (Maybe a, Cache, DynDeps)
runPrimTask (PrimTask x) = runRWST $ runMaybeT x

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
  debug = lift . AIO.debug
  readCommand cmd args = lift (AIO.readCommand cmd args)
  glob root pat = lift (AIO.glob root pat) <* tell (dependsOnPattern pat)

-- NOTE(flupe): ^ maybe for AchilleIO (PrimTask m) we actually want to do
--                smart path transformation?

withCache :: Monad m => Cache -> PrimTask m a -> PrimTask m (Maybe a, Cache)
withCache lcache (PrimTask (runMaybeT -> t)) =
  PrimTask $ MaybeT $ RWST \ctx cache -> do
    (x, lcache, deps) <- runRWST t ctx lcache
    pure (Just (x, lcache), cache, deps)

fromCache :: (Monad m, Binary a) => PrimTask m (Maybe a)
fromCache = Cache.fromCache <$> get

toCache :: (Monad m, Binary a) => a -> PrimTask m ()
toCache = put . Cache.toCache

splitCache :: Monad m => PrimTask m (Cache, Cache)
splitCache = Cache.splitCache <$> get

joinCache :: Monad m => Cache -> Cache -> PrimTask m ()
joinCache ca cb = put (Cache.joinCache ca cb)

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
