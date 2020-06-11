{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Achille.Task
    ( Task
    , match
    , match_
    , matchDir
    , with
    , watch
    , runTask
    ) where


import Data.Functor            ((<&>))
import Control.Monad           (forM)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Data.Binary             (Binary)
import System.FilePath         (FilePath, (</>), takeDirectory, takeFileName)
import System.FilePath.Glob    (Pattern)
import System.Directory
import System.IO               (openBinaryFile, hClose, IOMode(ReadMode))
import Data.Time.Clock         (UTCTime(..))
import Data.Time.Calendar      (Day(..))

import qualified System.FilePath.Glob as Glob
import qualified Data.ByteString.Lazy as ByteString

import Achille.Config (Config)
import Achille.Internal

import qualified Achille.Config as Config

-- what our tasks are caching
type MatchVoid  = [(FilePath, Cache)]
type Match b    = [(FilePath, (b, Cache))]
type MatchDir   = [(FilePath, Cache)]
type With a b   = (a, (b, Cache))
type Watch a b  = (a, Cache)


shouldForce :: Context a -> FilePath -> Bool
shouldForce ctx x = or (Glob.match <$> forceFiles ctx <*> pure x)

-- | Run a recipe on every filepath matching a given pattern.
--   The results are cached and the recipe only recomputes
--   when the underlying file has changed since last run.
match :: (MonadIO m, Binary a)
      => Pattern -> Recipe m FilePath a -> Recipe m c [a]
match p (Recipe r :: Recipe m FilePath b) = Recipe \ctx -> do
    let (cached, c'@Context{..}) = fromContext ctx
    paths <- liftIO $ withCurrentDirectory (inputDir </> currentDir) $
                      Glob.globDir1 p "" >>= mapM makeRelativeToCurrentDirectory
    case cached :: Maybe (Match b) of
        Nothing -> do
            result <- forM paths \p ->
                (p,) <$> r c' { inputValue = p
                              , cache = emptyCache
                              }
            return (map (fst . snd) result, toCache (result :: Match b))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just (v, cache) -> (p,) <$> do
                        tfile  <- liftIO $ getModificationTime (inputDir </> currentDir </> p)
                        if timestamp < tfile || shouldForce ctx (inputDir </> currentDir </> p) then
                            r c' {inputValue = p, cache = cache}
                        else pure (v, cache)
                    Nothing -> (p,) <$> r c' {inputValue = p, cache = emptyCache}
            return (map (fst . snd) result, toCache (result :: Match b))


-- | Run a recipe on every filepath matching a given pattern,
--   and discard the result.
--   Filepaths are cached and the recipe only recomputes when
--   the underlying file has changed since last run.
match_ :: MonadIO m => Pattern -> Recipe m FilePath a -> Task m ()
match_ p (Recipe r) = Recipe \ctx -> do
    let (result, c'@Context{..}) = fromContext ctx 
    paths <- liftIO $ withCurrentDirectory (inputDir </> currentDir) $
                      Glob.globDir1 p "" >>= mapM makeRelativeToCurrentDirectory
    case result :: Maybe MatchVoid of
        Nothing -> do
            result <- forM paths \p ->
                ((p,) . snd) <$> r c' { inputValue = p
                                      , cache = emptyCache
                                      }
            return (() , toCache (result :: MatchVoid))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just cache -> (p,) <$> do
                        tfile <- liftIO $ getModificationTime (inputDir </> currentDir </> p)
                        if timestamp < tfile || shouldForce ctx (inputDir </> currentDir </> p) then
                            snd <$> r c' {inputValue = p, cache = cache}
                        else pure cache
                    Nothing -> ((p,) . snd) <$> r c' {inputValue = p, cache = emptyCache}
            return ((), toCache (result :: MatchVoid))


-- | For every file matching the pattern, run a recipe with the
--   file as input and with the file's parent directory as current working directory.
--   The underlying recipe will be run regardless of whether the file was modified.
matchDir :: MonadIO m
         => Pattern -> Recipe m FilePath a -> Recipe m c [a]
matchDir p (Recipe r) = Recipe \ctx -> do
    let (result, c'@Context{..}) = fromContext ctx 
    paths <- liftIO $ withCurrentDirectory (inputDir </> currentDir) $
                      Glob.globDir1 p "" >>= mapM makeRelativeToCurrentDirectory
    case result :: Maybe MatchDir of
        Nothing -> do
            result <- forM paths \p ->
                          r c' { inputValue = takeFileName p
                               , currentDir = currentDir </> takeDirectory p
                               , cache      = emptyCache
                               }
            return (map fst result, toCache (zip paths (map snd result) :: MatchDir))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just cache -> r c' { inputValue = takeFileName p
                                       , currentDir = currentDir </> takeDirectory p
                                       , cache      = cache
                                       }
                    Nothing -> r c' {inputValue = p, cache = emptyCache}
            return (map fst result, toCache (zip paths (map snd result) :: MatchDir))


with :: (Applicative m, Binary a, Eq a, Binary b)
     => a -> Recipe m c b -> Recipe m c b
with (x :: a) (Recipe r :: Recipe m1 c d) = Recipe \ctx ->
    let (result, c'@Context{..}) = fromContext ctx 
    in case result :: Maybe (With a d) of
        Nothing ->
            r c' {cache = emptyCache}
                <&> \v -> (fst v, toCache ((x, v) :: With a d))
        Just (x', (v, cache)) ->
            if x == x' then pure (v , toCache ((x', (v, cache)) :: With a d))
            else r c' <&> \v -> (fst v, toCache ((x, v) :: With a d))


watch :: (Functor m, Binary a, Eq a)
      => a -> Recipe m c b -> Recipe m c b
watch (x :: a) (Recipe r :: Recipe m c b) = Recipe \ctx ->
    let (result, c'@Context{..}) = fromContext ctx
    in case result :: Maybe (Watch a b) of
        Nothing ->
            r c' {cache = emptyCache}
                <&> \v -> (fst v, toCache ((x, snd v) :: Watch a b))
        Just (x', cache) ->
            r c' {mustRun = if x /= x' then MustRunOne else NoMust, cache = cache}
                <&> \v -> (fst v, toCache ((x, snd v) :: Watch a b))


-- | Run a task using the provided config and a list of dirty files.
--   This takes care of loading the existing cache and updating it.
runTask :: MonadIO m
        => [Glob.Pattern]  -- ^ Files for which we force recompilation
        -> Config          -- ^ The config
        -> Task m a        -- ^ The task
        -> m a
runTask force config (Recipe r) = do
    cacheExists <- liftIO $ doesFileExist (Config.cacheFile config)
    timestamp   <- if cacheExists then
                        liftIO $ getModificationTime (Config.cacheFile config)
                   else pure (UTCTime (ModifiedJulianDay 0) 0)
    let ctx = Context (Config.contentDir config)
                      (Config.outputDir config)
                      ""
                      timestamp
                      force
                      NoMust
    (value, cache') <-
        if cacheExists then do
            handle <- liftIO $ openBinaryFile (Config.cacheFile config) ReadMode
            cache  <- liftIO $ ByteString.hGetContents handle
            let ret = r (ctx cache ())
            liftIO $ hClose handle
            ret
        else r (ctx emptyCache ())
    liftIO $ ByteString.writeFile (Config.cacheFile config) cache'
    pure value
