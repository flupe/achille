{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

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
  , Diffable(Diff, hasChanged)
  , Value
  , vArr
  , liftD
  ) where

import Control.Category.Constrained
import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import Data.Constraint
import Data.Constraint.Deferrable
import Data.Functor ((<&>))
import Data.Map (Map)
import GHC.Generics

import Data.Binary          qualified as Binary
import Data.ByteString.Lazy qualified as LBS

-- | Context in which a recipe is run.
data Context = Context
  {
  }

-- | The recipe abstraction.
-- A recipe runs in a given environment, using a local cache. It executes an
-- action in @m@, returns a value and updates the cache.
newtype Recipe m a b = Recipe { runRecipe :: Context -> Cache -> Value a -> m (Value b, Cache) }


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


-- * Diffing
--
-- $diffing
-- Some truly crude information about changes of values.

-- | Class for things that have a diff.
-- By default, we only know whether the value is new, not what it used to be
-- or how it has changed.
class Diffable a where
  type Diff a
  type Diff a = Bool

  -- | Given a value, tell whether it has changed since the last run.
  hasChanged :: Value a -> Bool
  default hasChanged :: (Diff a ~ Bool) => Value a -> Bool
  hasChanged (_, dx) = dx

  brandNew :: a -> Diff a
  default brandNew :: (Diff a ~ Bool) => a -> Diff a
  brandNew = const True

instance Diffable () where
  type Diff () = ()
  hasChanged _ = False
  brandNew = const ()

instance (Diffable a, Diffable b) => Diffable (a, b) where
  type Diff (a, b) = (Diff a, Diff b)

  hasChanged :: Value (a, b) -> Bool
  hasChanged ((x, y), (dx, dy)) = hasChanged (x, dx) || hasChanged (y, dy)

  brandNew :: (a, b) -> Diff (a, b)
  brandNew (x, y) = (brandNew x, brandNew y)

instance Diffable a => Diffable [a] where
  type Diff [a] = [Diff a]

  hasChanged :: Value [a] -> Bool
  hasChanged (xs, dxs) = any hasChanged (zip xs dxs)
    -- NOTE: this only works so long are they are the same length
    -- TODO: check if this is always the case
  
  brandNew :: [a] -> Diff [a]
  brandNew = map brandNew

-- | @Value a@ is an element of type @a@ along with information 
-- about how it changed since the last run.
type Value a = (a, Diff a)


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
  Recipe f × Recipe g = Recipe\ctx cache ((x, y), (dx, dy)) -> do
    let (cf, cg) = splitCache cache
    ((z, dz), cf') <- f ctx cf (x, dx)
    ((w, dw), cg') <- g ctx cg (y, dy)
    pure (((z, w), (dz, dw)), joinCache cf' cg')

  swap :: Recipe m (a ⊗ b) (b ⊗ a)
  swap = Recipe \ctx cache ((x, y), (dx, dy)) -> pure (((y, x), (dy, dx)), cache)

  assoc :: Recipe m ((a ⊗ b) ⊗ c) (a ⊗ (b ⊗ c))
  assoc = Recipe \ctx cache (((x, y), z), ((dx, dy), dz)) -> pure (((x, (y, z)), (dx, (dy, dz))), cache)

  assoc' :: Recipe m (a ⊗ (b ⊗ c)) ((a ⊗ b) ⊗ c)
  assoc' = Recipe \ctx cache ((x, (y, z)), (dx, (dy, dz))) -> pure ((((x, y), z), ((dx, dy), dz)), cache)

  unitor :: Recipe m a (a ⊗ ())
  unitor = Recipe \ctx cache (x, dx) -> pure (((x, ()), (dx, ())), cache)

  unitor' :: Recipe m (a ⊗ ()) a
  unitor' = Recipe \ctx cache ((x, ()), (dx, ())) -> pure ((x, dx), cache)


instance Monad m => Cartesian (Recipe m) where
  exl :: Recipe m (a ⊗ b) a
  exl = Recipe \ctx cache ((x, y), (dx, dy)) -> pure ((x, dx), cache)

  exr :: Recipe m (a ⊗ b) b
  exr = Recipe \ctx cache ((x, y), (dx, dy)) -> pure ((y, dy), cache)

  dup :: Recipe m a (a ⊗ a)
  dup = Recipe \ctx cache (x, dx) -> pure (((x, x), (dx, dx)), cache)

-- | A task is a recipe with no input.
type Task m = Recipe m ()

-- | Lift a task into a recipe accepting any input.
task :: Task m b -> Recipe m a b
task (Recipe r) = Recipe \cache ctx _ -> r cache ctx ((), ())

-- | Lift a pure function on /values/ to recipe.
vArr :: Applicative m => (Value a -> Value b) -> Recipe m a b
vArr f = Recipe \ctx cache va -> pure (f va, cache)

liftD :: (Functor m, Diffable a) => m a -> Task m a
liftD c = Recipe \_ cache _ -> c <&> \x -> ((x, brandNew x), cache)
