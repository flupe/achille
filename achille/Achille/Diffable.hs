{-# LANGUAGE DefaultSignatures #-}
module Achille.Diffable
  ( Diffable (..)
  , Value
  ) where

class Diffable a where
  type Diff a
  type Diff a = Bool

  -- | Given a value, tell whether it has changed since the last run.
  hasChanged :: Value a -> Bool
  default hasChanged :: (Diff a ~ Bool) => Value a -> Bool
  hasChanged (_, dx) = dx

  brandNew :: a -> Diff a
  default brandNew :: (Diff a ~ Bool) => a -> Diff a
  brandNew = const True

instance Diffable () where
  type Diff () = ()
  hasChanged _ = False
  brandNew = const ()

instance (Diffable a, Diffable b) => Diffable (a, b) where
  type Diff (a, b) = (Diff a, Diff b)

  hasChanged :: Value (a, b) -> Bool
  hasChanged ((x, y), (dx, dy)) = hasChanged (x, dx) || hasChanged (y, dy)

  brandNew :: (a, b) -> Diff (a, b)
  brandNew (x, y) = (brandNew x, brandNew y)

instance Diffable a => Diffable [a] where
  type Diff [a] = [Diff a]

  hasChanged :: Value [a] -> Bool
  hasChanged (xs, dxs) = any hasChanged (zip xs dxs)
    -- NOTE: this only works so long are they are the same length
    -- TODO: check if this is always the case
  
  brandNew :: [a] -> Diff [a]
  brandNew = map brandNew

-- | @Value a@ is an element of type @a@ along with information 
-- about how it changed since the last run.
type Value a = (a, Diff a)
