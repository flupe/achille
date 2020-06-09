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


type MatchVoid  = [(FilePath, Cache)]
type Match b    = [(FilePath, (b, Cache))]
type MatchDir   = [(FilePath, Cache)]
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


shouldForce :: Context a -> FilePath -> Bool
shouldForce ctx x = or (Glob.match <$> forceFiles ctx <*> pure x)

toRecipeContext :: Context a -> Recipe.Context a
toRecipeContext Context{..} = Recipe.Context inputDir outputDir inputValue

discardWhenMust :: Context b -> Maybe a -> Maybe a
discardWhenMust ctx x = if mustRun ctx then Nothing else x

runRecipe :: Context a -> Cache -> Recipe a b -> IO (b, Cache)


runRecipe ctx@Context{..} cache (MatchVoid p r) = do
    let cached = retrieveFromCache cache :: Maybe MatchVoid
    paths  <- withCurrentDirectory inputDir (Glob.globDir1 p "")
    case discardWhenMust ctx cached of
        Nothing -> do
            result <- forM paths \p -> ((p,) . snd) <$> runRecipe ctx {inputValue = p} emptyCache r
            return (() , asCache (result :: MatchVoid))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just cache -> (p,) <$> do
                        tfile <- getModificationTime (inputDir </> p)
                        if timestamp < tfile || shouldForce ctx (inputDir </> p) then
                            snd <$> runRecipe ctx {inputValue = p} cache r
                        else pure cache
                    Nothing -> ((p,) . snd) <$> runRecipe ctx {inputValue = p} emptyCache r
            return ((), asCache $ (result :: MatchVoid))


runRecipe ctx@Context{..} cache (Match p (r :: Recipe FilePath b)) = do
    let cached = retrieveFromCache cache :: Maybe (Match b)
    paths  <- withCurrentDirectory inputDir (Glob.globDir1 p "")
    case discardWhenMust ctx cached of
        Nothing -> do
            result <- forM paths \p -> (p,) <$> runRecipe ctx {inputValue = p} emptyCache r
            return (map (fst . snd) result, asCache $ (result :: Match b))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just (v, cache) -> (p,) <$> do
                        tfile  <- getModificationTime (inputDir </> p)
                        if timestamp < tfile || shouldForce ctx (inputDir </> p) then
                            runRecipe ctx {inputValue = p} cache r
                        else pure (v, cache)
                    Nothing -> (p,) <$> runRecipe ctx {inputValue = p} emptyCache r
            return (map (fst . snd) result, asCache $ (result :: Match b))


runRecipe ctx@Context{..} cache (MatchDir p (r :: Recipe FilePath b)) = do
    let cached = retrieveFromCache cache :: Maybe MatchDir
    paths  <- withCurrentDirectory inputDir (Glob.globDir1 p "")
    case discardWhenMust ctx cached of
        Nothing -> do
            result <- forM paths \p ->
                          runRecipe ctx { inputValue = p
                                        , inputDir   = inputDir </> p
                                        } emptyCache r
            return (map fst result, asCache $ (zip paths (map snd result) :: MatchDir))
        Just cached -> do
            result <- forM paths \p ->
                case lookup p cached of
                    Just cache -> runRecipe ctx { inputValue = p
                                                , inputDir   = inputDir </> p
                                                } cache r
                    Nothing -> runRecipe ctx {inputValue = p} emptyCache r
            return (map fst result, asCache $ (zip paths (map snd result) :: MatchDir))


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
