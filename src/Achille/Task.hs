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


import Data.Functor          ((<&>))
import Control.Monad         (forM)
import Data.Binary           (Binary)
import System.FilePath       (FilePath, (</>))
import System.FilePath.Glob  (Pattern)
import System.Directory
import System.IO             (openBinaryFile, hClose, IOMode(ReadMode))
import Data.Time.Clock       (UTCTime(..))
import Data.Time.Calendar    (Day(..))

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
match :: Binary a => Pattern -> Recipe FilePath a -> Task [a]
match p (Recipe r :: Recipe FilePath b) = Recipe \ctx -> do
    let (cached, c'@Context{..}) = fromContext ctx
    paths <- withCurrentDirectory inputDir (Glob.globDir1 p "")
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
                        tfile  <- getModificationTime (inputDir </> p)
                        if timestamp < tfile || shouldForce ctx (inputDir </> p) then
                            r c' {inputValue = p, cache = cache}
                        else pure (v, cache)
                    Nothing -> (p,) <$> r c' {inputValue = p, cache = emptyCache}
            return (map (fst . snd) result, toCache (result :: Match b))


-- | Run a recipe on every filepath matching a given pattern,
--   and discard the result.
--   Filepaths are cached and the recipe only recomputes when
--   the underlying file has changed since last run.
match_ :: Pattern -> Recipe FilePath a -> Task ()
match_ p (Recipe r) = Recipe \ctx -> do
    let (result, c'@Context{..}) = fromContext ctx 
    paths <- withCurrentDirectory inputDir (Glob.globDir1 p "")
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
                        tfile <- getModificationTime (inputDir </> p)
                        if timestamp < tfile || shouldForce ctx (inputDir </> p) then
                            snd <$> r c' {inputValue = p, cache = cache}
                        else pure cache
                    Nothing -> ((p,) . snd) <$> r c' {inputValue = p, cache = emptyCache}
            return ((), toCache (result :: MatchVoid))


-- | For every file matching the pattern, run a recipe with the
--   file as input and with the file's parent directory as current working directory.
--   The underlying recipe will be run regardless of whether the file was modified.
matchDir :: Pattern -> Recipe FilePath a -> Task [a]
matchDir p (Recipe r :: Recipe FilePath b) = Recipe \ctx -> do
    let (result, c'@Context{..}) = fromContext ctx 
    paths <- withCurrentDirectory inputDir (Glob.globDir1 p "")
    case result :: Maybe MatchDir of
        Nothing -> do
            result <- forM paths \p ->
                          r c' { inputValue = p
                               , inputDir   = inputDir </> p
                               , cache      = emptyCache
                               }
            return (map fst result, toCache (zip paths (map snd result) :: MatchDir))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just cache -> r c' { inputValue = p
                                       , inputDir   = inputDir </> p
                                       , cache      = cache
                                       }
                    Nothing -> r c' {inputValue = p, cache = emptyCache}
            return (map fst result, toCache (zip paths (map snd result) :: MatchDir))


with :: (Binary a, Eq a, Binary b) => a -> Task b -> Task b
with (x :: c) (Recipe r :: Recipe () b) = Recipe \ctx ->
    let (result, c'@Context{..}) = fromContext ctx 
    in case result :: Maybe (With c b) of
        Nothing ->
            r c' {cache = emptyCache}
                <&> \v -> (fst v, toCache ((x, v) :: With c b))
        Just (x', (v, cache)) ->
            if x == x' then pure (v , toCache ((x', (v, cache)) :: With c b))
            else r c' <&> \v -> (fst v, toCache ((x, v) :: With c b))


watch :: (Binary a, Eq a) => a -> Task b -> Task b
watch (x :: a) (Recipe r :: Task b) = Recipe \ctx ->
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
runTask :: [Glob.Pattern]  -- ^ Files for which we force recompilation
        -> Config          -- ^ The config
        -> Task a          -- ^ The task
        -> IO a
runTask force config (Recipe r) = do
    cacheExists <- doesFileExist (Config.cacheFile config)
    timestamp   <- if cacheExists then
                        getModificationTime (Config.cacheFile config)
                   else pure (UTCTime (ModifiedJulianDay 0) 0)
    let ctx = Context (Config.contentDir config)
                      (Config.outputDir config)
                      timestamp
                      force
                      NoMust
    (value, cache') <-
        if cacheExists then do
            handle <- openBinaryFile (Config.cacheFile config) ReadMode
            cache  <- ByteString.hGetContents handle
            ret    <- r (ctx cache ())
            hClose handle
            pure ret
        else r (ctx emptyCache ())
    ByteString.writeFile (Config.cacheFile config) cache'
    pure value
