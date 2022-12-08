{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : Achille.Syntax
Description : The EDSL syntax exposed to the user
Copyright   : (c) flupe, 2022
License     : MIT
Maintainer  : lucas@escot.me

This module exports the syntax available to the user to build recipes.
-}
module Achille.Syntax
  ( Port
  , (>>)
  , task
  , recipe
  -- * Basic operations
  , unit
  , void
  , debug
  -- * List operations
  , sort
  , sortOn
  , take
  , drop
  , chunks
  -- * Match operations
  , match
  , matchFile
  ) where

import Prelude (Ord, Monad, String, Int, (.), undefined)
import Control.Category.Constrained ((∘), exr)
import Control.Category.Linear hiding (unit)
import Data.Binary          (Binary)
import Data.String          (IsString(fromString))
import System.FilePath      (FilePath)
import System.FilePath.Glob (Pattern)

import Achille.Diffable (Diffable)
import Achille.IO       (AchilleIO)
import Achille.Recipe   (Recipe, Task)

import Control.Category.Linear qualified as Linear

import Achille.Recipe.Base     qualified as Recipe
import Achille.Recipe.List     qualified as Recipe
import Achille.Recipe.Match    qualified as Recipe


-- TODO: define Port as newtype, to not export the definition.
--       (and to avoid orphan instances)
-- | A @Port m r a@ represents the output of a recipe, computed in @m@, of type @a@.
type Port m r a = P (Recipe m) r a

instance (IsString a, Diffable a) => IsString (P (Recipe m) r a) where
  fromString = undefined

-- | Sequence two outputs by discarding the first one. 
-- Intended to be used with @QualifiedDo@ to easily discard values.
(>>) :: Monad m => Port m r a %1 -> Port m r b %1 -> Port m r b
x >> y = ignore (discard x) y

-- TODO: see if we can define a typeclass sor that each of the following names
-- resolves to either @Recipe ...@ or @Port .. %1 -> Port ...@

void :: Monad m => Port m r a %1 -> Port m r ()
void = discard

-- | Convert a /closed/ port into a task.
task :: Monad m => (forall r. Port m r b) -> Task m b
task r = exr ∘ decode \x -> merge (x, r)

-- | Convert a /closed/ linear function between ports into a recipe.
recipe :: Monad m => (forall r. Port m r a %1 -> Port m r b) -> Recipe m a b
recipe = decode

unit :: Monad m => Port m r ()
unit = Linear.unit

debug :: (Monad m, AchilleIO m) => Port m r String %1 -> Port m r ()
debug = encode Recipe.debug

sort :: (Monad m, Ord a) => Port m r [a] %1 -> Port m r [a]
sort = encode Recipe.sort

sortOn :: (Monad m, Ord b) => (a -> b) -> Port m r [a]  %1 -> Port m r [a]
sortOn = encode . Recipe.sortOn

take :: Monad m => Int -> Port m r [a] %1 -> Port m r [a]
take = encode . Recipe.take

drop :: Monad m => Int -> Port m r [a] %1 -> Port m r [a]
drop = encode . Recipe.drop

chunks :: Monad m => Int -> Port m r [a] %1 -> Port m r [[a]]
chunks = encode . Recipe.chunks

-- NOTE: this @forall r.@ quantification inside the function means we cannot use variables bound outside of it.
--       but that is precisely what I want to allow.
--       should think carefully about that tomorrow
-- match :: (Monad m, AchilleIO m, Diffable b) => Pattern -> (FilePath -> forall r. Port m r b) %1 -> Port m r [b]
match :: (Monad m, AchilleIO m, Binary b) => Pattern -> (forall r. Port m r FilePath %1 -> Port m r b) -> Port m r [b]
match pat f = encode (Recipe.match pat (recipe f)) unit

matchFile :: (Monad m, AchilleIO m, Binary b) => FilePath -> Port m r b %1 -> Port m r b
matchFile = undefined
