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

import Achille.Diffable (joinList, diff)
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
  (result, caches) <- unzip <$> forM paths \p -> runRecipe t ctx emptyCache (p, diff p False)
  return (joinList result, cache)
