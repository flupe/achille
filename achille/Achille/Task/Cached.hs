{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}

-- | Defines core combinators for processing files incrementally.
module Achille.Task.Cached
    ( Task
    , match
    , match_
    , matchFile
    , matchDir
    , with
    , watch
    , runTask
    , shouldForce
    , shouldConsider
    ) where


import Data.Functor            ((<&>))
import Control.Monad           (forM, filterM)
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
import Achille.Internal.IO (AchilleIO)
import qualified Achille.Internal.IO as AchilleIO

-- what our tasks are caching
type MatchVoid  = [(FilePath, Cache)]
type Match b    = [(FilePath, (b, Cache))]
type MatchDir   = [(FilePath, Cache)]
type With a b   = (a, (b, Cache))
type Watch a    = (a, Cache)


shouldForce :: Context -> FilePath -> Bool
shouldForce ctx x = or (Glob.match <$> forceFiles ctx <*> pure x)

shouldConsider :: [Pattern] -> FilePath -> Bool
shouldConsider pats p =
    not (or [Glob.match pat p | pat <- pats])

-- TODO: investigate whether we need glob at all?
--       It doesn't look like it's very fast,
--       and it doesn't allow you to do conditionals with {}
--       such as *.{jpg, png}

-- | Run a recipe on every filepath matching a given pattern.
--   The results are cached and the recipe only recomputes
--   when the underlying file has changed since last run.
match :: (AchilleIO m, Binary a)
      => Pattern -> (FilePath -> Task m a) -> Task m [a]
match pattern (t :: FilePath -> Task m b) = Task \ctx -> do
    let (cached, c'@Context{..}) = fromContext ctx
    paths <- AchilleIO.glob (inputDir </> currentDir) pattern
             <&> filter (shouldConsider ignore)
             >>= filterM (AchilleIO.doesFileExist . ((inputDir </> currentDir) </>))
    case cached :: Maybe (Match b) of
        Nothing -> do
            result <- forM paths \p ->
                (p,) <$> unTask (t p) c' { cache = emptyCache }
            return (map (fst . snd) result, toCache (result :: Match b))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just (v, cache) -> (p,) <$> do
                        tfile  <- AchilleIO.getModificationTime (inputDir </> currentDir </> p)
                        if timestamp < tfile || shouldForce ctx (inputDir </> currentDir </> p) then
                            unTask (t p) c' {cache = cache}
                        else pure (v, cache)
                    Nothing -> (p,) <$> unTask (t p) c' {cache = emptyCache}
            return (map (fst . snd) result, toCache (result :: Match b))


-- | Run a task on every filepath matching a given pattern,
--   and discard the result.
--   Filepaths are cached and the task only recomputes when
--   the underlying file has changed since last run.
match_ :: AchilleIO m => Pattern -> (FilePath -> Task m a) -> Task m ()
match_ pattern t = Task \ctx -> do
    let (result, c'@Context{..}) = fromContext ctx 
    paths <- AchilleIO.glob (inputDir </> currentDir) pattern
             <&> filter (shouldConsider ignore)
             >>= filterM (AchilleIO.doesFileExist . ((inputDir </> currentDir) </>))
    case result :: Maybe MatchVoid of
        Nothing -> do
            result <- forM paths \p ->
                ((p,) . snd) <$> unTask (t p) c' {cache = emptyCache}
            return (() , toCache (result :: MatchVoid))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just cache -> (p,) <$> do
                        tfile <- AchilleIO.getModificationTime (inputDir </> currentDir </> p)
                        if timestamp < tfile || shouldForce ctx (inputDir </> currentDir </> p) then
                            snd <$> unTask (t p) c' {cache = cache}
                        else pure cache
                    Nothing -> ((p,) . snd) <$> unTask (t p) c' {cache = emptyCache}
            return ((), toCache (result :: MatchVoid))


-- | Run a recipe for a filepath matching a given pattern.
--   The result is cached and the recipe only recomputes
--   when the underlying file has changed since last run.
--   Will fail is no file is found matching the pattern.
matchFile :: (AchilleIO m, Binary a)
          => Pattern -> (FilePath -> Task m a) -> Task m a
matchFile p (t :: FilePath -> Task m a) = Task \ctx@Context{..} ->
    AchilleIO.glob (inputDir </> currentDir) p >>= \case
        [] -> AchilleIO.fail $ unwords
                  [ "No file was found matching pattern"
                  , Glob.decompile p
                  , "inside directory"
                  , currentDir
                  ]
        (p:xs) ->
            let (result, c'@Context{..}) = fromContext ctx
            in case result :: Maybe (Watch a) of
                Nothing -> unTask (t p) c' {cache = emptyCache}
                               <&> \v -> (fst v, toCache (v :: Watch a))
                Just (x, cache) -> do
                    tfile  <- AchilleIO.getModificationTime (inputDir </> currentDir </> p)
                    if timestamp < tfile || shouldForce ctx (inputDir </> currentDir </> p) then
                        unTask (t p) c' {cache = cache}
                            <&> \v -> (fst v, toCache (v :: Watch a))
                    else pure (x, toCache ((x, cache) :: Watch a))


-- | For every file matching the pattern, run a recipe with the
--   file as input and with the file's parent directory as current working directory.
--   The underlying recipe will be run regardless of whether the file was modified.
matchDir :: AchilleIO m
         => Pattern -> (FilePath -> Task m a) -> Task m [a]
matchDir pattern t = Task \ctx -> do
    let (result, c'@Context{..}) = fromContext ctx 
    paths <- AchilleIO.glob (inputDir </> currentDir) pattern
    case result :: Maybe MatchDir of
        Nothing -> do
            result <- forM paths \p ->
                          unTask (t (takeFileName p)) c'
                              { currentDir = currentDir </> takeDirectory p
                              , cache      = emptyCache
                              }
            return (map fst result, toCache (zip paths (map snd result) :: MatchDir))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just cache -> unTask (t (takeFileName p)) c'
                                      { currentDir = currentDir </> takeDirectory p
                                      , cache      = cache
                                      }
                    Nothing -> unTask (t p) c' {cache = emptyCache}
            return (map fst result, toCache (zip paths (map snd result) :: MatchDir))


-- | Cache a value and only trigger a given recipe if said value has changed between runs.
--   cache the result of the recipe.
with :: (Applicative m, Binary a, Eq a, Binary b)
     => a -> Task m b -> Task m b
with (x :: a) (t :: Task m1 d) = Task \ctx ->
    let (result, c'@Context{..}) = fromContext ctx 
    in case result :: Maybe (With a d) of
        Nothing ->
            unTask t c' {cache = emptyCache}
                <&> \v -> (fst v, toCache ((x, v) :: With a d))
        Just (x', (v, cache)) ->
            if x == x' then pure (v , toCache ((x', (v, cache)) :: With a d))
            else unTask t c' <&> \v -> (fst v, toCache ((x, v) :: With a d))


-- | Cache a value and only trigger a given recipe if said value has changed between runs.
--   Like 'with', but the result of the recipe won't be cached.
--   If the recipe must be retriggered, it will be in depth.
watch :: (Functor m, Binary a, Eq a)
      => a -> Task m b -> Task m b
watch (x :: a) (t :: Task m b) = Task \ctx ->
    let (result, c'@Context{..}) = fromContext ctx
    in case result :: Maybe (Watch a) of
        Nothing ->
            unTask t c' {cache = emptyCache}
                <&> \v -> (fst v, toCache ((x, snd v) :: Watch a))
        Just (x', cache) ->
            unTask t c' {mustRun = if x /= x' then MustRunOne else NoMust, cache = cache}
                <&> \v -> (fst v, toCache ((x, snd v) :: Watch a))


-- | Run a task using the provided config and a list of dirty files.
--   This takes care of loading the existing cache and updating it.
runTask :: MonadIO m
        => [Glob.Pattern]  -- ^ Files for which we force recompilation
        -> Config          -- ^ The config
        -> Task m a        -- ^ The task
        -> m a
runTask force config t = do
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
                      (Config.ignore config)
    (v, cache') <-
        if cacheExists then do
            handle <- liftIO $ openBinaryFile (Config.cacheFile config) ReadMode
            cache  <- liftIO $ ByteString.hGetContents handle
            (value, cache') <- unTask t (ctx cache)
            liftIO $ hClose handle
            pure (value, cache')
        else do unTask t (ctx emptyCache)
    liftIO $ ByteString.writeFile (Config.cacheFile config) cache'
    pure v
