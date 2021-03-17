{-# LANGUAGE DeriveFunctor    #-}

-- | Defines recipes, how they compose and evaluate.
module Achille.Internal
    ( Cache
    , emptyCache
    , toCache
    , fromCache
    , fromContext
    , MustRun(..)
    , Context(..)
    , Task(..)
    , nonCached
    ) where

import Prelude hiding (fail, liftIO)

import Data.Binary             (Binary, encode, decodeOrFail)
import Data.Maybe              (fromMaybe)
import Data.Functor            (void)
import Control.Monad           (ap)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Fail      (MonadFail, fail)
import Control.Applicative     (liftA2)
import Data.Time.Clock         (UTCTime)
import Data.ByteString.Lazy    (ByteString, empty)
import Data.Bifunctor          (first, second)
import System.FilePath.Glob    (Pattern)


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
fromContext :: Binary a => Context -> (Maybe a, Context)
fromContext c =
    let r = mustRun c in
    if r /= NoMust then (Nothing, c {mustRun = lowerMustRun r})
    else (fromCache (cache c), c)



-- | Description of a computation producing a value b given some input a.


-- | Context in which a recipe is being executed.
data Context = Context
    { inputDir    :: FilePath        -- ^ Input root directory
    , outputDir   :: FilePath        -- ^ Output root directory
    , currentDir  :: FilePath        -- ^ Current directory
    , timestamp   :: UTCTime         -- ^ Timestamp of the last run
    , forceFiles  :: [Pattern]       -- ^ Files marked as dirty
    , mustRun     :: MustRun         -- ^ Whether the current task must run
    , ignore      :: [Pattern]
    , cache       :: Cache           -- ^ Local cache
    }


-- | A task is a recipe with no input
newtype Task m a = Task { unTask :: Context -> m (a, Cache) }


-- | Make a recipe out of a computation that is known not to be cached.
nonCached :: Functor m => (Context -> m a) -> Task m a
nonCached f = Task \c -> (, emptyCache) <$> f c {cache = emptyCache}


instance Functor m => Functor (Task m) where
    fmap f (Task r) = Task \c -> first f <$> r c


instance Monad m => Applicative (Task m) where
    pure  = Task . const . pure . (, emptyCache)
    (<*>) = ap


splitCache :: Cache -> (Cache, Cache)
splitCache = fromMaybe (emptyCache, emptyCache) . fromCache


instance Monad m => Monad (Task m) where
    Task r >>= f = Task \c -> do
        let (cr, cf) = splitCache (cache c)
        (x, cr') <- r c {cache = cr}
        (y, cf') <- unTask (f x) c {cache = cf}
        pure (y, toCache (cr', cf'))

    -- parallelism for free?
    Task r >> Task s = Task \c -> do
        let (cr, cs) = splitCache (cache c)
        (_, cr') <- r c {cache = cr}
        (y, cs') <- s c {cache = cs}
        pure (y, toCache (cr', cs'))


instance MonadIO m => MonadIO (Task m) where
    liftIO = nonCached . const . liftIO


instance MonadFail m => MonadFail (Task m) where
    fail = Task . const . fail


instance (Monad m, Semigroup a) => Semigroup (Task m a) where
    x <> y = liftA2 (<>) x y


instance (Monad m, Monoid a) => Monoid (Task m a) where
    mempty = pure mempty
