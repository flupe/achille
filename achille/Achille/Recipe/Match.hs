-- | Recipes for matching on files.
module Achille.Recipe.Match 
  ( match
  ) where

import Control.Monad (forM, forM_)
import Data.Functor ((<&>))
import Data.Map (Map)
import System.FilePath.Glob (Pattern)

import Data.Map             qualified as Map
import System.FilePath.Glob qualified as Glob

import Achille.IO (AchilleIO(glob))
import Achille.Recipe (Recipe(..), Cache, emptyCache)

type Match b = Map FilePath (b, Cache)

-- TODO: caching
-- TODO: parallelize

-- | Run a recipe for every filepath matching a given pattern.
match :: (Monad m, AchilleIO m)
      => Pattern -> Recipe m FilePath b -> Recipe m a [b]
match pat t = Recipe \ctx cache x -> do
  paths <- glob "." pat
  result <- unzip <$> forM paths \p -> fst <$> runRecipe t ctx emptyCache (p, undefined)
  return (result, cache)

-- | Run a recipe for every filepath matching a given pattern.
match_ :: (Monad m, AchilleIO m)
      => Pattern -> (FilePath -> Recipe m a b) -> Recipe m a ()
match_ pat (t :: FilePath -> Recipe m a b) = Recipe \ctx cache x -> do
  paths <- glob "." pat
  forM_ paths \p -> runRecipe (t p) ctx emptyCache x
  return (((), ()), cache)
