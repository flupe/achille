{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Achille.Run
    ( Cache
    , runTaskCached
    , run
    ) where


import Data.Functor          ((<&>))
import Data.Maybe            (fromMaybe)
import Data.Binary           (Binary, put, get)
import Data.ByteString.Lazy  (ByteString)
import Control.Monad         (forM, forM_, when, void)
import System.IO             (openBinaryFile, hClose, IOMode(ReadMode))
import System.FilePath       ((</>))
import System.Directory

import qualified System.FilePath.Glob as Glob
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Binary          as Binary

import Achille.Config
import Achille.Recipe
import Achille.Task


-- | Our cache is simply a lazy bytestring
type Cache = ByteString

emptyCache :: Cache
emptyCache = ByteString.empty

type CacheMatchVoid = [FilePath]
type CacheMatch b   = [(FilePath, b)]
type CacheWith a b  = (a, (b, Cache))


-- | Try to load a value from the cache
retrieveFromCache :: Binary a => Cache -> Maybe a
retrieveFromCache cache =
    case Binary.decodeOrFail cache of
        Left _          -> Nothing
        Right (_, _, x) -> Just x

-- | Encode a value
asCache :: Binary a => a -> Cache
asCache = Binary.encode


runTaskCached :: Cache -> Task a -> IO (a, Cache)
runTaskCached cache (TaskMatchVoid p (Recipe r)) =
    case retrieveFromCache cache :: Maybe CacheMatchVoid of
      Nothing -> do
        paths  <- withCurrentDirectory contentDir (Glob.globDir1 p "")
        mapM r paths >> return (() , asCache paths)
      Just paths' -> do
        paths  <- withCurrentDirectory contentDir (Glob.globDir1 p "")
        forM_ paths \p ->
            when (elem p paths') do
                tcache <- getModificationTime cacheFile
                tfile  <- getModificationTime (contentDir </> p)
                when (tcache < tfile) (void $ r p)
        return ((), asCache $ paths)


runTaskCached cache (TaskMatch p (Recipe r :: Recipe FilePath b)) =
    case retrieveFromCache cache :: Maybe (CacheMatch b) of
      Nothing -> do
        paths  <- withCurrentDirectory contentDir (Glob.globDir1 p "")
        values <- mapM r paths
        return (values , asCache $ zip paths values)
      Just cached -> do
        paths  <- withCurrentDirectory contentDir (Glob.globDir1 p "")
        cache' <- forM paths \p ->
            case lookup p cached of
                Just v  -> do
                    tcache <- getModificationTime cacheFile
                    tfile  <- getModificationTime (contentDir </> p)
                    if tcache < tfile then (p,) <$> r p
                    else pure (p , v)
                Nothing -> (p,) <$> r p 
        return (map snd cache', asCache $ cache')


runTaskCached cache (TaskWith (x :: b) (t :: Task a) :: Task a) =
    case retrieveFromCache cache :: Maybe (CacheWith b a) of
        Nothing -> runTaskCached cache t <&> \v -> (fst v, asCache (x, v))
        Just (x', v) ->
            if x == x' then pure (fst v , cache)
            else runTaskCached cache t <&> \v -> (fst v, asCache (x, v))


runTaskCached cache (TaskIO x) = (, emptyCache) <$> x

runTaskCached cache (TaskBind t f) = do
    let (tcache, fcache) = fromMaybe (emptyCache, emptyCache)
                         $ retrieveFromCache cache
    (vt, tcache') <- runTaskCached tcache t
    (vf, fcache') <- runTaskCached fcache (f vt)
    return (vf , asCache (tcache', fcache'))


-- | Main runner, takes care of loading and updating the cache
run :: Task a -> IO a
run t = do
    cacheExists <- doesFileExist cacheFile
    (value, cache') <-
        if cacheExists then do
            handle <- openBinaryFile cacheFile ReadMode
            cache  <- ByteString.hGetContents handle
            ret    <- runTaskCached cache t
            hClose handle
            pure ret
        else runTaskCached emptyCache t
    ByteString.writeFile cacheFile cache'
    pure value
