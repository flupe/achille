-- | Recipes for matching on files.
module Achille.Recipe.Match 
  ( match
  ) where

import Control.Monad (forM)
import Data.Functor ((<&>))
import Data.Map (Map)
import System.FilePath.Glob (Pattern)

import Data.Map             qualified as Map
import System.FilePath.Glob qualified as Glob

import Achille.IO (AchilleIO(glob))
import Achille.Recipe (Recipe(..), Cache, emptyCache, Diffable)

type Match b = Map FilePath (b, Cache)

-- TODO: caching
-- | Run a recipe for every filepath matching a given pattern.
match :: (Monad m, AchilleIO m, Diffable b) 
      => Pattern -> (FilePath -> Recipe m a b) -> Recipe m a [b]
match pat (t :: FilePath -> Recipe m a b) = Recipe \ctx cache x -> do
  paths <- glob "." pat
  result <- unzip <$> forM paths \p -> fst <$> runRecipe (t p) ctx emptyCache x
  return (result, cache)
