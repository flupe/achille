{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs #-}

module Achille.Run
    ( Cache
    , Context(Context)
    , runRecipe
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
import Achille.Task
import Achille.Recipe hiding (Context, inputDir, outputDir)
import Achille.Internal hiding (Context(..))

import qualified Achille.Recipe as Recipe (Context(..))
import qualified Achille.Config as Config (Config(..))


-- | Our cache is simply a lazy bytestring
type Cache = ByteString

emptyCache :: Cache
emptyCache = ByteString.empty


type MatchVoid  = [FilePath]
type Match b    = [(FilePath, (b, Cache))]
type With a b   = (a, (b, Cache))
type Watch a b  = (a, Cache)


-- | Try to load a value from the cache
retrieveFromCache :: Binary a => Cache -> Maybe a
retrieveFromCache cache =
    case Binary.decodeOrFail cache of
        Left _          -> Nothing
        Right (_, _, x) -> Just x


-- | Encode a value
asCache :: Binary a => a -> Cache
asCache = Binary.encode


data Context a = Context
    { inputDir    :: FilePath        -- ^ Current input directory
    , outputDir   :: FilePath        -- ^ Current output directory
    , timestamp   :: UTCTime         -- ^ Timestamp of the last run
    , forceFiles  :: [Glob.Pattern]  -- ^ Files whose recompilation we force
    , mustRun     :: Bool            -- ^ Whether the current task should be forced
    , inputValue  :: a
    } deriving (Functor)


shouldForce :: Context a -> FilePath -> Bool
shouldForce ctx x = or (Glob.match <$> forceFiles ctx <*> pure x)

toRecipeContext :: Context a -> Recipe.Context a
toRecipeContext Context{..} = Recipe.Context inputDir outputDir inputValue

discardWhenMust :: Context b -> Maybe a -> Maybe a
discardWhenMust ctx x = if mustRun ctx then Nothing else x

runRecipe :: Context a -> Cache -> Recipe a b -> IO (b, Cache)

runRecipe ctx@Context{..} cache (MatchVoid p r) =
    let cached = retrieveFromCache cache :: Maybe MatchVoid
    in case discardWhenMust ctx cached of
      Nothing -> do
        paths <- withCurrentDirectory inputDir (Glob.globDir1 p "")
        forM_ paths \p -> runRecipe ctx {inputValue = p} cache r
        return (() , asCache (paths :: MatchVoid))
      Just paths' -> do
        paths <- withCurrentDirectory inputDir (Glob.globDir1 p "")
        forM_ paths \p ->
            when (elem p paths' || shouldForce ctx (inputDir </> p)) do
                tfile <- getModificationTime (inputDir </> p)
                when (timestamp < tfile) (void $ runRecipe ctx {inputValue = p} emptyCache r)
        return ((), asCache $ (paths :: MatchVoid))

runRecipe ctx@Context{..} cache (Match p (r :: Recipe FilePath b)) =
    let cached = retrieveFromCache cache :: Maybe (Match b)
    in case discardWhenMust ctx cached of
      Nothing -> do
        paths  <- withCurrentDirectory inputDir (Glob.globDir1 p "")
        values <- forM paths \p -> runRecipe ctx {inputValue = p} emptyCache r
        return (map fst values, asCache $ (zip paths values :: Match b))
      Just cached -> do
        paths  <- withCurrentDirectory inputDir (Glob.globDir1 p "")
        cache' <- forM paths \p ->
            case lookup p cached of
                Just v  -> do
                    tfile  <- getModificationTime (inputDir </> p)
                    if timestamp < tfile || shouldForce ctx (inputDir </> p) then
                        runRecipe ctx {inputValue = p} emptyCache r
                    else pure v
                Nothing -> runRecipe ctx {inputValue = p} emptyCache r
        return (map fst cache', asCache $ (zip paths cache' :: Match b))

runRecipe ctx@Context{..} cache (MatchDir p (r :: Recipe FilePath b)) = do
    paths  <- withCurrentDirectory inputDir (Glob.globDir1 p "")
    values <- forM paths \p ->
                  runRecipe ctx { inputValue = p
                                , inputDir   = inputDir </> p
                                } cache r
    -- TODO: fix
    return (map fst values, emptyCache)

runRecipe ctx cache (With (x :: c) (r :: Recipe a b)) =
    let cached = retrieveFromCache cache :: Maybe (With c b)
    in case discardWhenMust ctx cached of
        Nothing -> runRecipe ctx cache r
                       <&> \v -> (fst v, asCache ((x, v) :: With c b))
        Just (x', (v, cache)) ->
            if x == x' then pure (v , asCache ((x', (v, cache)) :: With c b))
            else runRecipe ctx cache r
                     <&> \v -> (fst v, asCache ((x, v) :: With c b))

runRecipe ctx cache (Watch (x :: c) (t :: Recipe a b)) =
    let cached = retrieveFromCache cache :: Maybe (Watch c b)
    in case discardWhenMust ctx cached of
        Nothing -> runRecipe ctx cache t
                       <&> \v -> (fst v, asCache ((x, snd v) :: Watch c b))
        Just (x', cache) -> runRecipe (ctx {mustRun = x /= x'}) cache t
                          <&> \v -> (fst v, asCache ((x, snd v) :: Watch c b))

runRecipe ctx _ (RunIO r) =
    (, emptyCache) <$> r (toRecipeContext ctx)

runRecipe ctx _ (Return x) = pure (x, emptyCache)

runRecipe ctx cache (Bind t r) = do
    let (tcache, fcache) = fromMaybe (emptyCache, emptyCache)
                         $ retrieveFromCache cache
    (vt, tcache') <- runRecipe ctx tcache t
    (vf, fcache') <- runRecipe ctx fcache (r vt)
    return (vf , asCache (tcache', fcache'))


-- | Main runner, takes care of loading and updating the cache
run :: [Glob.Pattern]  -- ^ Files for which we force recompilation
    -> Config          -- ^ Global config
    -> Task a          -- ^ The task to be run
    -> IO a
run force config t = do
    cacheExists <- doesFileExist (Config.cacheFile config)
    timestamp   <- if cacheExists then
                        getModificationTime (Config.cacheFile config)
                   else pure (UTCTime (ModifiedJulianDay 0) 0)
    let ctx = Context (Config.contentDir config)
                      (Config.outputDir config)
                      timestamp
                      force
                      False
                      ()
    (value, cache') <-
        if cacheExists then do
            handle <- openBinaryFile (cacheFile config) ReadMode
            cache  <- ByteString.hGetContents handle
            ret    <- runRecipe ctx cache t
            hClose handle
            pure ret
        else runRecipe ctx emptyCache t
    ByteString.writeFile (cacheFile config) cache'
    pure value
