{-# LANGUAGE GADTs               #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TupleSections       #-}

module Task where

import System.FilePath
import System.Directory

import Data.Functor    ((<&>))
import Data.Typeable   (Typeable)
import Data.Binary     (Binary, decodeFile, encodeFile)
import Data.Dynamic.Binary
import Control.Monad   (liftM, liftM2)
import Control.Applicative
import Data.Maybe (fromMaybe)

import qualified System.FilePath.Glob as Glob

import Recipe
import Config
import Cache


-- | Description of a task producing an intermediate value of type a
data Task a where
    TaskMatch  :: (Typeable a, Binary a) => Glob.Pattern -> Recipe FilePath a -> Task [a]
    TaskWith   :: (Typeable a, Binary a, Eq a, Typeable b, Binary b) => a -> Recipe a b -> Task b
    TaskIO     :: IO a -> Task a
    TaskBind   :: Task a -> (a -> Task b) -> Task b


-- | Run a task as is, no incremental builds
-- runTask :: Task a -> IO a
-- runTask (TaskMatch p (Recipe r)) = 
--     withCurrentDirectory contentDir (Glob.globDir1 p "")
--         >>= mapM r
-- runTask (TaskWith x (Recipe r)) = r x
-- runTask (TaskBind ta f) = runTask ta >>= runTask . f
-- runTask (TaskIO x) = x


runTaskCached :: (Typeable a, Binary a) => Cache -> Task a -> IO (a, Cache)
runTaskCached cache (TaskMatch p (Recipe r :: Recipe FilePath b)) =
    case fromCache cache :: Maybe (CacheMatch b) of
      -- nothing is cached
      Nothing -> do
        paths  <- withCurrentDirectory contentDir (Glob.globDir1 p "")
        values <- mapM r paths
        return (values , toCache $ zip paths values)

      -- some paths are in cache
      Just cached -> do
        -- TODO: inspect changes
        return (map snd cached, cache)

runTaskCached cache (TaskWith (x :: b) (Recipe r :: Recipe b a)) =
    case fromCache cache :: Maybe (CacheWith b a) of
        Nothing -> r x <&> \v -> (v, toCache (x, v))
        Just (x', v) ->
            if x == x' then pure (v , cache)
            else r x <&> \v -> (v, toCache (x, v))

runTaskCached cache (TaskIO x) = (,cache) <$> x

runTaskCached cache (TaskBind t@(TaskMatch p (r :: Recipe FilePath a)) f) = do
    let (tcache, fcache) = fromMaybe (emptyCache, emptyCache) $
                               fromCache cache
    (vt, tcache') <- runTaskCached tcache t
    (vf, fcache') <- runTaskCached fcache (f vt)
    return (vf , toCache (tcache', fcache'))

runTaskCached cache (TaskBind t@(TaskWith (x :: b) (_ :: Recipe b a)) f) = do
    let (tcache, fcache) = fromMaybe (emptyCache, emptyCache) $
                               fromCache cache
    (vt, tcache') <- runTaskCached tcache t
    (vf, fcache') <- runTaskCached fcache (f vt)
    return (vf , toCache (tcache', fcache'))

runTaskCached cache (TaskBind t@(TaskIO x) f) = x >>= runTaskCached cache . f

runTaskCached cache (TaskBind (TaskBind ta f) g) = 
    runTaskCached cache (TaskBind ta \a -> TaskBind (f a) g)



-- | Main runner, takes care of loading and updating the cache
achille :: (Typeable a, Binary a) => Task a -> IO a
achille t = do
    cacheExists <- doesFileExist cacheFile
    cache <- if cacheExists then putStrLn "Found cache. loading..."
                              >> decodeFile cacheFile
                            else pure $ toDyn ()
    (value, cache') <- runTaskCached cache t
    putStrLn "Updating cache..."
    encodeFile cacheFile cache'
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

match :: (Typeable a, Binary a) => Glob.Pattern -> Recipe FilePath a -> Task [a]
match = TaskMatch

with :: (Typeable a, Binary a, Eq a, Typeable b, Binary b) => a -> Recipe a b -> Task b
with = TaskWith
