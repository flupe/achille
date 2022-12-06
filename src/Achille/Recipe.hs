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
module Achille.Recipe where

import Prelude
import Data.Map hiding ((\\))
import Data.Binary
import Data.ByteString.Lazy as LBS
import GHC.Generics
import qualified Data.Binary as Binary
import Control.Category.Constrained
import Data.Constraint
import Data.Constraint.Deferrable

-- | Context in which a recipe is run.
data Context = Context
  { tagged :: Map String Cache -- ^ A map of caches, used for expensive computations that 
                               -- should be made insensitive to code refactoring
  }

-- | The recipe abstraction.
-- A recipe runs in a given environment, using a local cache. It executes an
-- action in @m@, returns a value and updates the cache.
newtype Recipe m a b = Recipe { runRecipe :: Context -> Cache -> Value a -> m (Value b, Cache) }

-- | A task is a recipe that takes no input.
type Task m = Recipe m ()


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

  subDiffable :: forall b c. (Diffable (b, c), a ~ (b, c)) => Dict (Diffable b, Diffable c)
  subDiffable = error "not a product"

instance Diffable () where
  type Diff () = ()
  hasChanged _ = False

instance (Diffable a, Diffable b) => Diffable (a, b) where
  type Diff (a, b) = (Diff a, Diff b)
  subDiffable = Dict
  hasChanged ((x, y), (dx, dy)) = hasChanged (x, dx) || hasChanged (y, dy)

instance ProdObj Diffable where
  objprod :: forall z a b. (z ~ (a, b), Diffable z) => Dict (Diffable a, Diffable b)
  objprod = subDiffable

  prodobj :: (Diffable a, Diffable b) => Dict (Diffable (a, b))
  prodobj = Dict

  objunit :: Dict (Diffable ())
  objunit = Dict

type Value a = (a, Diff a)


instance Monad m => Category (Recipe m) where
  type Obj (Recipe m) = Diffable

  id :: Diffable a => Recipe m a a
  id = Recipe \ctx cache v -> pure (v, cache)

  (∘) :: (Diffable a, Diffable b, Diffable c) => Recipe m b c -> Recipe m a b -> Recipe m a c
  Recipe g ∘ Recipe f = Recipe \ctx cache vx -> do
    let (cf, cg) = splitCache cache
    (vy, cf') <- f ctx cf vx
    (vz, cg') <- g ctx cg vy
    pure (vz, joinCache cf' cg')


instance Monad m => Monoidal (Recipe m) where
  (×) :: (Diffable a, Diffable b, Diffable c, Diffable d)
      => Recipe m a b -> Recipe m c d -> Recipe m (a ⊗ c) (b ⊗ d)
  Recipe f × Recipe g = Recipe\ctx cache ((x, y), (dx, dy)) -> do
    let (cf, cg) = splitCache cache
    ((z, dz), cf') <- f ctx cf (x, dx)
    ((w, dw), cg') <- g ctx cg (y, dy)
    pure (((z, w), (dz, dw)), joinCache cf' cg')

  swap :: (Diffable a, Diffable b) => Recipe m (a ⊗ b) (b ⊗ a)
  swap = Recipe \ctx cache ((x, y), (dx, dy)) -> pure (((y, x), (dy, dx)), cache)

  assoc :: (Diffable a, Diffable b, Diffable c) => Recipe m ((a ⊗ b) ⊗ c) (a ⊗ (b ⊗ c))
  assoc = Recipe \ctx cache (((x, y), z), ((dx, dy), dz)) -> pure (((x, (y, z)), (dx, (dy, dz))), cache)

  assoc' :: (Diffable a, Diffable b, Diffable c) => Recipe m (a ⊗ (b ⊗ c)) ((a ⊗ b) ⊗ c)
  assoc' = Recipe \ctx cache ((x, (y, z)), (dx, (dy, dz))) -> pure ((((x, y), z), ((dx, dy), dz)), cache)

  unitor :: Diffable a => Recipe m a (a ⊗ ())
  unitor = Recipe \ctx cache (x, dx) -> pure (((x, ()), (dx, ())), cache)

  unitor' :: Diffable a => Recipe m (a ⊗ ()) a
  unitor' = Recipe \ctx cache ((x, ()), (dx, ())) -> pure ((x, dx), cache)


instance Monad m => Cartesian (Recipe m) where
  exl :: (Diffable a, Diffable b) => Recipe m (a ⊗ b) a
  exl = Recipe \ctx cache ((x, y), (dx, dy)) -> pure ((x, dx), cache)

  exr :: (Diffable a, Diffable b) => Recipe m (a ⊗ b) b
  exr = Recipe \ctx cache ((x, y), (dx, dy)) -> pure ((y, dy), cache)

  dup :: Diffable a => Recipe m a (a ⊗ a)
  dup = Recipe \ctx cache (x, dx) -> pure (((x, x), (dx, dx)), cache)
