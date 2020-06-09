{-# LANGUAGE DeriveFunctor    #-}

module Achille.Internal
    ( Cache
    , emptyCache
    , toCache
    , fromCache
    , fromContext
    , MustRun(..)
    , Context(..)
    , Recipe(..)
    , Task
    , runRecipe
    , nonCached
    ) where


import Data.Binary           (Binary, encode, decodeOrFail)
import Data.Maybe            (fromMaybe)
import Control.Monad         (liftM2)
import Control.Applicative   (liftA2)
import Data.Time.Clock       (UTCTime)
import Data.ByteString.Lazy  (ByteString, empty)
import Data.Bifunctor        (first, second)
import System.FilePath.Glob  (Pattern)


-- | A cache is a lazy bytestring.
type Cache = ByteString


-- | The empty cache.
emptyCache :: Cache
emptyCache = empty

-- | Cache a value.
toCache :: Binary a => a -> Cache
toCache = encode

-- | Retrieve a value from cache.
fromCache :: Binary a => Cache -> Maybe a
fromCache cache =
    case decodeOrFail cache of
        Left _          -> Nothing
        Right (_, _, x) -> Just x


-- | Local rules for running a recipe
data MustRun = MustRunOne  -- ^ The current recipe, and only this one, must run
             | MustRunAll  -- ^ All subsequent recipes must run
             | NoMust      -- ^ No obligation, the current recipe will be run as normal
             deriving (Eq)


lowerMustRun :: MustRun -> MustRun
lowerMustRun MustRunAll = MustRunAll
lowerMustRun x = NoMust

-- | Try to load a value from the cache,
--   while respecting the rule for running the recipe.
--   That is, if the rule must run, nothing will be returned.
--   We also lower the run rule in the returned context, if possible.
--
--   The types are not explicit enough, should rewrite.
fromContext :: Binary a => Context b -> (Maybe a, Context b)
fromContext c =
    let r = mustRun c in
    if r /= NoMust then (Nothing, c {mustRun = lowerMustRun r})
    else (fromCache (cache c), c)



-- | Description of a computation producing a value b given some input a.
newtype Recipe a b = Recipe (Context a -> IO (b, Cache))


-- | Context in which a recipe is being executed.
data Context a = Context
    { inputDir    :: FilePath        -- ^ Current working directory
    , outputDir   :: FilePath        -- ^ Current output directory
    , timestamp   :: UTCTime         -- ^ Timestamp of the last run
    , forceFiles  :: [Pattern]       -- ^ Files marked as dirty
    , mustRun     :: MustRun         -- ^ Whether the current task must run
    , cache       :: Cache           -- ^ Local cache
    , inputValue  :: a               -- ^ Input value
    } deriving (Functor)


-- | A task is a recipe with no input
type Task = Recipe ()


-- our recipes are a little bit of everything, that's good
instance Functor (Recipe a) where
    fmap f (Recipe r) = Recipe \c -> first f <$> r c


instance Applicative (Recipe a) where
    pure = Recipe . const . pure . (, emptyCache)
    liftA2 = liftM2


splitCache :: Cache -> (Cache, Cache)
splitCache = fromMaybe (emptyCache, emptyCache) . fromCache


instance Monad (Recipe a) where
    Recipe r >>= f = Recipe \c -> do
        let (cr, cf) = splitCache (cache c)
        (x, cr') <- r c   {cache = cr}
        (y, cf') <- runRecipe (f x) c {cache = cf}
        pure (y, toCache (cr', cf'))

    -- parallelism for free?
    Recipe r >> Recipe s = Recipe \c -> do
        let (cr, cs) = splitCache (cache c)
        (_, cr') <- r c {cache = cr}
        (y, cs') <- s c {cache = cs}
        pure (y, toCache (cr', cs'))


runRecipe :: Recipe a b -> Context a -> IO (b, Cache)
runRecipe (Recipe r) = r


-- | Make a recipe out of a computation that is known not to be cached.
nonCached :: (Context a -> IO b) -> Recipe a b
nonCached f = Recipe \c -> (, emptyCache) <$> f c {cache = emptyCache}
