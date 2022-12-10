{-# LANGUAGE Rank2Types #-}
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
  ( module Achille.Syntax.Core
  -- * Basic operations
  , void
  , debug
  -- * List operations
  , sort
  , sortOn
  , take
  , drop
  --, chunks
  -- * Match operations
  , match
  , matchFile
  ) where

import Prelude hiding ((>>), take, drop, sort)
import Data.Binary          (Binary)
import Data.String          (IsString(fromString))
import System.FilePath      (FilePath)
import System.FilePath.Glob (Pattern)

import Achille.Diffable (unitV, diff)
import Achille.IO       (AchilleIO)
import Achille.Recipe   (Recipe, Task, pureV)

import Achille.Syntax.Core

import Achille.Recipe.Base     qualified as Recipe
import Achille.Recipe.List     qualified as Recipe
import Achille.Recipe.Match    qualified as Recipe


instance (Monad m, IsString a) => IsString (Port m r a) where
  fromString s = apply (pureV (fromString s) False) unit

-- | Discard an output value.
void :: Monad m => Port m r a -> Port m r ()
void x = x >> unit

debug :: (Monad m, AchilleIO m) => Port m r String -> Port m r ()
debug = apply Recipe.debug

sort :: (Monad m, Ord a) => Port m r [a] -> Port m r [a]
sort = apply Recipe.sort

sortOn :: (Monad m, Ord b) => (a -> b) -> Port m r [a] -> Port m r [a]
sortOn = apply . Recipe.sortOn

take :: Monad m => Int -> Port m r [a] -> Port m r [a]
take = apply . Recipe.take

drop :: Monad m => Int -> Port m r [a] -> Port m r [a]
drop = apply . Recipe.drop

-- chunks :: Monad m => Int -> Port m r [a] %1 -> Port m r [[a]]
-- chunks = encode . Recipe.chunks

-- NOTE: this @forall r.@ quantification inside the function means we cannot use variables bound outside of it.
--       but that is precisely what I want to allow.
--       should think carefully about that tomorrow
-- match :: (Monad m, AchilleIO m, Diffable b) => Pattern -> (FilePath -> forall r. Port m r b) %1 -> Port m r [b]
match :: (Monad m, AchilleIO m, Binary b) => Pattern -> (forall r. Port m r FilePath -> Port m r b) -> Port m r [b]
match pat f = apply (Recipe.match pat (recipe f)) unit

matchFile :: (Monad m, AchilleIO m, Binary b) => FilePath -> Port m r b -> Port m r b
matchFile = undefined
