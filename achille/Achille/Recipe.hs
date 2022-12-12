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
  , vArr
  , liftD
  , pureV
  , valueToTask
  , void
    -- * @Recipe m@ is a monoidal category
    --
    -- $monoidal
  , (×)
  , swap
  , assoc
  , assoc'
  , unitor
  , unitor'
    -- * @Recipe m@ is a cartesian category
    --
    -- $cartesian
  , exl
  , exr
  , dup
  , (▵)
  ) where

import Control.Category
import Prelude hiding ((.))
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

import Achille.Cache
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


instance Monad m => Category (Recipe m) where
  id :: Recipe m a a
  id = Recipe \ctx cache v -> pure (v, cache)

  (.) :: Recipe m b c -> Recipe m a b -> Recipe m a c
  Recipe g . Recipe f = Recipe \ctx cache vx -> do
    let (cf, cg) = splitCache cache
    (vy, cf') <- f ctx cf vx
    (vz, cg') <- g ctx cg vy
    pure (vz, joinCache cf' cg')

-- $monoidal

(×) :: Monad m => Recipe m a b -> Recipe m c d -> Recipe m (a, c) (b, d)
Recipe f × Recipe g = Recipe\ctx cache v -> do
  let (cf, cg) = splitCache cache
  let (vx, vy) = splitPair v
  (vz, cf') <- f ctx cf vx
  (vw, cg') <- g ctx cg vy
  pure (joinPair vz vw, joinCache cf' cg')

swap :: Applicative m => Recipe m (a, b) (b, a)
swap = Recipe \ctx cache v ->
  let (vx, vy) = splitPair v in pure (joinPair vy vx, cache)

assoc :: Applicative m => Recipe m ((a, b), c) (a, (b, c))
assoc = Recipe \ctx cache v ->
  let ((vx, vy), vz) = first splitPair $ splitPair v
  in pure (joinPair vx (joinPair vy vz), cache)

assoc' :: Applicative m => Recipe m (a, (b, c)) ((a, b), c)
assoc' = Recipe \ctx cache v ->
  let (vx, (vy, vz)) = second splitPair $ splitPair v
  in pure (joinPair (joinPair vx vy) vz, cache)

unitor :: Applicative m => Recipe m a (a, ())
unitor = Recipe \ctx cache v -> pure (joinPair v unitV, cache)

unitor' :: Applicative m => Recipe m (a, ()) a
unitor' = Recipe \ctx cache v -> pure (fst (splitPair v), cache)


-- $cartesian

exl :: Applicative m => Recipe m (a, b) a
exl = Recipe \ctx cache v -> pure (fst (splitPair v), cache)

exr :: Applicative m => Recipe m (a, b) b
exr = Recipe \ctx cache v -> pure (snd (splitPair v), cache)

dup :: Applicative m => Recipe m a (a, a)
dup = Recipe \ctx cache v -> pure (joinPair v v, cache)

(▵) :: Monad m => Recipe m a b -> Recipe m a c -> Recipe m a (b, c)
f ▵ g = (f × g) . dup

-- | Lift a pure function on /values/ to recipe.
vArr :: Applicative m => (Value a -> Value b) -> Recipe m a b
vArr f = Recipe \ctx cache v -> pure (f v, cache)

liftD :: (Functor m) => m a -> Task m a
liftD c = Recipe \_ cache _ -> c <&> \x -> ((x, diff x True), cache)

valueToTask :: Applicative m => Value a -> Task m a
valueToTask = vArr . const

pureV :: Applicative m => a -> Bool -> Task m a
pureV x b = vArr $ const (x, diff x b)

void :: Applicative m => Recipe m a ()
void = Recipe \_ cache _ -> pure (unitV, cache)
