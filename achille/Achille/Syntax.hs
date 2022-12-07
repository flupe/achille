{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Achille.Syntax
  ( Port
  , (>>)
  , task
  , debug
  -- * List operations
  , sort
  , sortOn
  , take
  , drop
  , chunks
  -- * Match operations
  , match
  ) where

import Prelude (Ord, Monad, String, Int, (.), undefined)
import Achille.IO (AchilleIO)
import Achille.Recipe (Recipe, Task, Diffable)
import Control.Category.Constrained ((∘), exr)
import Control.Category.Linear
import System.FilePath (FilePath)
import System.FilePath.Glob (Pattern)

import Achille.Recipe.Base  qualified as Recipe
import Achille.Recipe.List  qualified as Recipe
import Achille.Recipe.Match qualified as Recipe

type Port m r a = P (Recipe m) r a

-- TODO: see if we can define a typeclass sor that each of the following names
-- resolves to either @Recipe ...@ or @Port .. %1 -> Port ...@

task :: Monad m => (forall r. Port m r b) -> Task m b
task r = exr ∘ decode \x -> merge (x, r)

debug :: (Monad m, AchilleIO m) => String -> Port m r ()
debug msg = encode (Recipe.debug msg) unit

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
match :: (Monad m, AchilleIO m, Diffable b) => Pattern -> (FilePath -> forall r. Port m r b) -> Port m r [b]
match pat f = encode (Recipe.match pat (\src -> task (f src))) unit

-- | Can be used with @QualifiedDo@ to easily discard values
(>>) :: Monad m => Port m r a %1 -> Port m r b %1 -> Port m r b
x >> y = ignore (discard x) y
