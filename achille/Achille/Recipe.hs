{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Achille.Recipe
Description : Basic building blocks for the @Recipe@ abstraction
Copyright   : (c) flupe, 2022
License     : MIT
Maintainer  : lucas@escot.me

This module defines the @Recipe m@ abstraction and associated properties.
-}
module Achille.Recipe
  ( Context(..)
  , Recipe(..)
  , Task
  , task
  , Cache
  , emptyCache
  , splitCache
  , joinCache
  , fromCache
  , toCache
  , vArr
  , liftD
  , pureV
  ) where

import Control.Category.Constrained
import Data.Bifunctor (first, second)
import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import Data.Constraint
import Data.Constraint.Deferrable
import Data.Functor ((<&>))
import Data.Map (Map)
import GHC.Generics

import Data.Binary          qualified as Binary
import Data.ByteString.Lazy qualified as LBS

import Achille.Diffable


-- | Context in which a recipe is run.
data Context = Context
  {
  }

-- | The recipe abstraction.
-- A recipe runs in a given environment, using a local cache. It executes an
-- action in @m@, returns a value and updates the cache.
newtype Recipe m a b = Recipe { runRecipe :: Context -> Cache -> Value a -> m (Value b, Cache) }

-- | A task is a recipe with no input.
type Task m = Recipe m ()

-- | Lift a task into a recipe accepting any input.
task :: Task m b -> Recipe m a b
task (Recipe r) = Recipe \cache ctx _ -> r cache ctx unitV



-- * Cache
--
-- $cache
-- All recipes are run with a local cache that they can use freely to remember
-- information between runs.

-- | The cache received by recipes. It is a list in order to make composition associative.
newtype Cache = Cache { chunks :: [ByteString] } deriving (Generic, Binary)

-- | The empty cache.
emptyCache :: Cache
emptyCache = Cache []

-- | Splits the cache in two.
splitCache :: Cache -> (Cache, Cache)
splitCache (Cache []) = (emptyCache, emptyCache)
splitCache (Cache (c:cs)) = (Binary.decode c, Cache cs)

-- | Combines two caches.
joinCache :: Cache -> Cache -> Cache
joinCache hd (Cache cs) = Cache (Binary.encode hd : cs)

-- | Retrieve a value from cache.
fromCache :: Binary a => Cache -> Maybe a
fromCache (Cache []) = Nothing
fromCache (Cache (c:cs)) =
  case Binary.decodeOrFail c of
    Left _ -> Nothing
    Right (_, _, x) -> Just x

-- | Writes a value to cache.
toCache :: Binary a => a -> Cache
toCache x = Cache [Binary.encode x]



instance Monad m => Category (Recipe m) where
  id :: Recipe m a a
  id = Recipe \ctx cache v -> pure (v, cache)

  (∘) :: Recipe m b c -> Recipe m a b -> Recipe m a c
  Recipe g ∘ Recipe f = Recipe \ctx cache vx -> do
    let (cf, cg) = splitCache cache
    (vy, cf') <- f ctx cf vx
    (vz, cg') <- g ctx cg vy
    pure (vz, joinCache cf' cg')


instance Monad m => Monoidal (Recipe m) where
  (×) :: Recipe m a b -> Recipe m c d -> Recipe m (a ⊗ c) (b ⊗ d)
  Recipe f × Recipe g = Recipe\ctx cache v -> do
    let (cf, cg) = splitCache cache
    let (vx, vy) = splitPair v
    (vz, cf') <- f ctx cf vx
    (vw, cg') <- g ctx cg vy
    pure (joinPair vz vw, joinCache cf' cg')

  swap :: Recipe m (a ⊗ b) (b ⊗ a)
  swap = Recipe \ctx cache v ->
    let (vx, vy) = splitPair v in pure (joinPair vy vx, cache)

  assoc :: Recipe m ((a ⊗ b) ⊗ c) (a ⊗ (b ⊗ c))
  assoc = Recipe \ctx cache v ->
    let ((vx, vy), vz) = first splitPair $ splitPair v
    in pure (joinPair vx (joinPair vy vz), cache)

  assoc' :: Recipe m (a ⊗ (b ⊗ c)) ((a ⊗ b) ⊗ c)
  assoc' = Recipe \ctx cache v ->
    let (vx, (vy, vz)) = second splitPair $ splitPair v
    in pure (joinPair (joinPair vx vy) vz, cache)

  unitor :: Recipe m a (a ⊗ ())
  unitor = Recipe \ctx cache v -> pure (joinPair v unitV, cache)

  unitor' :: Recipe m (a ⊗ ()) a
  unitor' = Recipe \ctx cache v -> pure (fst (splitPair v), cache)


instance Monad m => Cartesian (Recipe m) where
  exl :: Recipe m (a ⊗ b) a
  exl = Recipe \ctx cache v -> pure (fst (splitPair v), cache)

  exr :: Recipe m (a ⊗ b) b
  exr = Recipe \ctx cache v -> pure (snd (splitPair v), cache)

  dup :: Recipe m a (a ⊗ a)
  dup = Recipe \ctx cache v -> pure (joinPair v v, cache)

-- | Lift a pure function on /values/ to recipe.
vArr :: Applicative m => (Value a -> Value b) -> Recipe m a b
vArr f = Recipe \ctx cache v -> pure (f v, cache)


liftD :: (Functor m) => m a -> Task m a
liftD c = Recipe \_ cache _ -> c <&> \x -> ((x, diff x True), cache)

pureV :: Applicative m => a -> Bool -> Task m a
pureV x b = vArr $ const (x, diff x b)
