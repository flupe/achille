{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}

module Achille.Syntax.Core
  ( Port
  , unit
  , split
  , merge
  , apply
  , recipe
  , (>>=)
  , (>>)
  ) where


import Achille.Diffable (unitV)
import Achille.Recipe as Recipe

import Prelude hiding ((.),id,curry, (>>=), (>>))
import Control.Category


-- | A value of type @Port m r a@ represents the output of a recipe in `m`,
--   carrying data of type @a@.
newtype Port m r a = Y { fromP :: Recipe m r a }

-- | A port void of information.
unit :: Applicative m => Port m r ()
unit = Y void

-- | apply a recipe to a port.
apply  :: Monad m => Recipe m a b -> Port m r a -> Port m r b
apply φ (Y f) = Y (φ . f)

-- | Split a port carrying pairs into two ports carrying the projections.
--   Beware, split /will/ duplicate the input port, and thus its attached producer recipe.
--   You should always use it as a view-pattern along with @(>>=)@, so that the incoming
--   recipe is executed before splitting.
split  :: Monad m => Port m r (a, b) -> (Port m r a, Port m r b)
split (Y f) = (Y (exl . f), Y (exr . f))

-- | Merge a pair of ports into a port carrying the pair.
merge  :: Monad m => (Port m r a, Port m r b) -> Port m r (a, b)
merge (Y f, Y g) = Y (f ▵ g)

-- | Convert any *closed* function over ports into a recipe.
recipe :: Monad m => (forall r. Port m r a -> Port m r b) -> Recipe m a b
recipe f = fromP (f (Y id))

-- | Bind operation over ports. The is intended to be used with the `QualifiedDo`
--   language extension, to have a readable syntax for binding variables.
(>>=) :: Monad m => Port m r a -> (Port m r a -> Port m r b) -> Port m r b
Y x >>= f = Y $ Recipe \ctx cache v -> do
  let (cx, cf) = splitCache cache
  (a, cx) <- runRecipe x ctx cx v -- we force evaluation here
  (z, cf) <- runRecipe (fromP $ f $ Y $ Recipe.task $ valueToTask a) ctx cf v
  pure (z, joinCache cx cf)
  -- TODO: investigate whether this will be problematic when parallelizing.

(>>) :: Monad m => Port m r a -> Port m r b -> Port m r b
Y x >> Y y = Y (exr . (x ▵ y))
-- TODO: make sequencing a primitive of Recipe m, to avoid constructing tuples for nothing
