{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Achille.Run
    ( Cache
    , Context(Context)
    , runTaskCached
    , run
    ) where


import Data.Functor          ((<&>))
import Data.Maybe            (fromMaybe)
import Data.Binary           (Binary, put, get)
import Data.ByteString.Lazy  (ByteString)
import Data.Time.Clock       (UTCTime(..))
import Data.Time.Calendar    (Day(..))
import Control.Monad         (forM, forM_, when, void)
import System.IO             (openBinaryFile, hClose, IOMode(ReadMode))
import System.FilePath       ((</>))
import System.Directory

import qualified System.FilePath.Glob as Glob
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Binary          as Binary

import Achille.Config hiding (outputDir)
import Achille.Recipe hiding (Context, inputDir, outputDir)
import Achille.Task

import qualified Achille.Recipe as Recipe (Context(..))
import qualified Achille.Config as Config (Config(..))


-- | Our cache is simply a lazy bytestring
type Cache = ByteString

emptyCache :: Cache
emptyCache = ByteString.empty

type CacheMatchVoid = [FilePath]
type CacheMatch b   = [(FilePath, b)]
type CacheWith a b  = (a, (b, Cache))
type CacheWatch a b  = (a, Cache)


-- | Try to load a value from the cache
retrieveFromCache :: Binary a => Cache -> Maybe a
retrieveFromCache cache =
    case Binary.decodeOrFail cache of
        Left _          -> Nothing
        Right (_, _, x) -> Just x

-- | Encode a value
asCache :: Binary a => a -> Cache
asCache = Binary.encode


data Context = Context
    { inputDir   :: FilePath        -- ^ Current input directory
    , outputDir  :: FilePath        -- ^ Current output directory
    , timestamp  :: UTCTime         -- ^ Timestamp of the last run
    , forceFiles :: [Glob.Pattern]  -- ^ Files whose recompilation we force
    , mustRun    :: Bool            -- ^ Whether the current task should be forced
    }

shouldForce :: Context -> FilePath -> Bool
shouldForce ctx x = or (Glob.match <$> forceFiles ctx <*> pure x)

toRecipeContext :: Context -> a -> Recipe.Context a
toRecipeContext Context{..} x = Recipe.Context inputDir outputDir x

discardWhenMust :: Context -> Maybe a -> Maybe a
discardWhenMust ctx x = if mustRun ctx then Nothing else x

runTaskCached :: Context -> Cache -> Task a -> IO (a, Cache)
runTaskCached ctx@Context{..} cache (TaskMatchVoid p (Recipe r)) =
    let cached = retrieveFromCache cache :: Maybe CacheMatchVoid
    in case discardWhenMust ctx cached of
      Nothing -> do
        paths <- withCurrentDirectory inputDir (Glob.globDir1 p "")
        mapM (r . toRecipeContext ctx) paths >> return (() , asCache paths)
      Just paths' -> do
        paths <- withCurrentDirectory inputDir (Glob.globDir1 p "")
        forM_ paths \p ->
            when (elem p paths' || shouldForce ctx (inputDir </> p)) do
                tfile <- getModificationTime (inputDir </> p)
                when (timestamp < tfile) (void $ r $ toRecipeContext ctx p)
        return ((), asCache $ paths)


runTaskCached ctx@Context{..} cache (TaskMatch p (Recipe r :: Recipe FilePath b)) =
    let cached = retrieveFromCache cache :: Maybe (CacheMatch b)
    in case discardWhenMust ctx cached of
      Nothing -> do
        paths  <- withCurrentDirectory inputDir (Glob.globDir1 p "")
        values <- mapM (r . toRecipeContext ctx) paths
        return (values , asCache $ zip paths values)
      Just cached -> do
        paths  <- withCurrentDirectory inputDir (Glob.globDir1 p "")
        cache' <- forM paths \p ->
            case lookup p cached of
                Just v  -> do
                    tfile  <- getModificationTime (inputDir </> p)
                    if timestamp < tfile || shouldForce ctx (inputDir </> p) then
                        (p,) <$> r (toRecipeContext ctx p)
                    else pure (p , v)
                Nothing -> (p,) <$> r (toRecipeContext ctx p)
        return (map snd cache', asCache $ cache')


runTaskCached ctx cache (TaskWith (x :: b) (t :: Task a)) =
    let cached = retrieveFromCache cache :: Maybe (CacheWith b a)
    in case discardWhenMust ctx cached of
        Nothing -> runTaskCached ctx cache t <&> \v -> (fst v, asCache (x, v))
        Just (x', v) ->
            if x == x' then pure (fst v , cache)
            else runTaskCached ctx cache t <&> \v -> (fst v, asCache (x, v))

runTaskCached ctx cache (TaskWatch (x :: b) (t :: Task a)) =
    let cached = retrieveFromCache cache :: Maybe (CacheWatch b a)
    in case discardWhenMust ctx cached of
        Nothing      -> runTaskCached ctx cache t
                          <&> \v -> (fst v, asCache (x, snd v))
        Just (x', cache) -> runTaskCached(ctx {mustRun = x /= x'}) cache t
                          <&> \v -> (fst v, asCache (x, snd v))


runTaskCached ctx cache (TaskRecipe (Recipe r)) =
    (, emptyCache) <$> r (toRecipeContext ctx ())

runTaskCached ctx cache (TaskBind t f) = do
    let (tcache, fcache) = fromMaybe (emptyCache, emptyCache)
                         $ retrieveFromCache cache
    (vt, tcache') <- runTaskCached ctx tcache t
    (vf, fcache') <- runTaskCached ctx fcache (f vt)
    return (vf , asCache (tcache', fcache'))


-- | Main runner, takes care of loading and updating the cache
run :: [Glob.Pattern]  -- ^ Files for which we force recompilation
    -> Config          -- ^ Global config
    -> Task a          -- ^ The task to be run
    -> IO a
run force config t = do
    print force
    cacheExists <- doesFileExist (Config.cacheFile config)
    timestamp   <- if cacheExists then
                        getModificationTime (Config.cacheFile config)
                   else pure (UTCTime (ModifiedJulianDay 0) 0)
    let ctx = Context (Config.contentDir config)
                      (Config.outputDir config)
                      timestamp
                      force
                      False
    (value, cache') <-
        if cacheExists then do
            handle <- openBinaryFile (cacheFile config) ReadMode
            cache  <- ByteString.hGetContents handle
            ret    <- runTaskCached ctx cache t
            hClose handle
            pure ret
        else runTaskCached ctx emptyCache t
    ByteString.writeFile (cacheFile config) cache'
    pure value
