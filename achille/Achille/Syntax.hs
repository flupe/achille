{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LinearTypes #-}
module Achille.Syntax
  ( module Control.Category.Linear
  , recipe
  , (>>)
  ) where

import Prelude hiding ((>>))
import Achille.Recipe (Recipe, Task)
import Control.Category.Constrained
import Control.Category.Linear

recipe :: Monad m => (forall r. P (Recipe m) r b) -> Task m b
recipe r = exr ∘ decode \x -> merge (x, r)

-- | Can be used with @QualifiedDo@ to easily discard values
(>>) :: Monad m => P (Recipe m) r a %1 -> P (Recipe m) r b %1 -> P (Recipe m) r b
x >> y = ignore (discard x) y
