{-# LANGUAGE GADTs               #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TupleSections       #-}

module Task where

import System.FilePath
import System.Directory
import System.IO             (openBinaryFile, hClose, IOMode(ReadMode))

import Data.Functor          ((<&>))
import Control.Monad         (liftM, liftM2, forM)
import Control.Applicative
import Data.List             (lookup)
import Data.Either           (fromRight)
import Data.Maybe            (fromMaybe)
import Data.Time.Clock       (UTCTime)

import Data.Binary           (Binary, get, put)
import Data.ByteString.Lazy  (ByteString)

import qualified System.FilePath.Glob as Glob
import qualified Data.Binary          as Binary
import qualified Data.ByteString.Lazy as ByteString

import Recipe
import Config



-- | Description of a task producing an intermediate value of type a
data Task a where
    TaskMatch  :: Binary a => Glob.Pattern -> Recipe FilePath a -> Task [a]
    TaskWith   :: (Binary a, Eq a, Binary b) => a -> Task b -> Task b
    TaskIO     :: IO a -> Task a
    TaskBind   :: Task a -> (a -> Task b) -> Task b


-- | Our cache is simply a lazy bytestring
type Cache = ByteString

emptyCache :: Cache
emptyCache = ByteString.empty

-- | Content of a TaskMatch task stored in cache
data CacheMatch b = CacheMatch [(FilePath, b)]

-- | Content of a TaskWith task stored in cache
data CacheWith a b = CacheWith a (b, Cache)


instance Binary b => Binary (CacheMatch b) where
    put (CacheMatch x) = put x
    get = CacheMatch <$> get

instance (Binary a, Binary b) => Binary (CacheWith a b) where
    put (CacheWith x y) = put x >> put y
    get = CacheWith <$> get <*> get

-- | Try to load a value from the cache
retrieveFromCache :: Binary a => Cache -> Maybe a
retrieveFromCache cache =
    case Binary.decodeOrFail cache of
        Left _          -> Nothing
        Right (_, _, x) -> Just x

-- | Encode a value
asCache :: Binary a => a -> Cache
asCache = Binary.encode

runTaskCached :: Binary a => Cache -> Task a -> IO (a, Cache)
runTaskCached cache (TaskMatch p (Recipe r :: Recipe FilePath b)) =
    case retrieveFromCache cache :: Maybe (CacheMatch b) of
      Nothing -> do
        paths  <- withCurrentDirectory contentDir (Glob.globDir1 p "")
        values <- mapM r paths
        return (values , asCache $ CacheMatch $ zip paths values)
      Just (CacheMatch cached) -> do
        paths  <- withCurrentDirectory contentDir (Glob.globDir1 p "")
        cache' <- forM paths \p ->
            case lookup p cached of
                Just v  -> do
                    tcache <- getModificationTime cacheFile
                    tfile  <- getModificationTime (contentDir </> p)
                    if tcache < tfile then (p,) <$> r p
                    else pure (p , v)
                Nothing -> (p,) <$> r p 
        return (map snd cache', asCache $ CacheMatch $ cache')

runTaskCached cache (TaskWith (x :: b) (t :: Task a) :: Task a) =
    case retrieveFromCache cache :: Maybe (CacheWith b a) of
        Nothing -> runTaskCached cache t <&> \v -> (fst v, asCache $ CacheWith x v)
        Just (CacheWith x' v) ->
            if x == x' then pure (fst v , cache)
            else runTaskCached cache t <&> \v -> (fst v, asCache $ CacheWith x v)

runTaskCached cache (TaskIO x) = (,cache) <$> x

runTaskCached cache (TaskBind t@(TaskMatch _ _) f) = do
    let (tcache, fcache) = fromMaybe (emptyCache, emptyCache) $ retrieveFromCache cache
    (vt, tcache') <- runTaskCached tcache t
    (vf, fcache') <- runTaskCached fcache (f vt)
    return (vf , asCache (tcache', fcache'))

runTaskCached cache (TaskBind t@(TaskWith _ _) f) = do
    let (tcache, fcache) = fromMaybe (emptyCache, emptyCache) $ retrieveFromCache cache
    (vt, tcache') <- runTaskCached tcache t
    (vf, fcache') <- runTaskCached fcache (f vt)
    return (vf , asCache (tcache', fcache'))

runTaskCached cache (TaskBind t@(TaskIO x) f) = x >>= runTaskCached cache . f

runTaskCached cache (TaskBind (TaskBind ta f) g) = 
    runTaskCached cache (TaskBind ta \a -> TaskBind (f a) g)


-- | Main runner, takes care of loading and updating the cache
achille :: Binary a => Task a -> IO a
achille t = do
    cacheExists <- doesFileExist cacheFile
    (value, cache') <-
        if cacheExists then do
            handle <- openBinaryFile cacheFile ReadMode
            cache  <- ByteString.hGetContents handle
            ret <- runTaskCached cache t
            hClose handle
            pure ret
        else runTaskCached emptyCache t
    ByteString.writeFile cacheFile cache'
    pure value


instance Functor Task where
    fmap = liftM

instance Applicative Task where
    pure   = return
    liftA2 = liftM2

instance Monad Task where
    return = TaskIO . pure
    (>>=)  = TaskBind


------------------
-- Task builders

match :: Binary a => Glob.Pattern -> Recipe FilePath a -> Task [a]
match = TaskMatch

with :: (Binary a, Eq a, Binary b) => a -> Task b -> Task b
with = TaskWith
