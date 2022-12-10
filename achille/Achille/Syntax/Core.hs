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


import Achille.Recipe

import Prelude hiding ((.),id,curry, (>>=), (>>))
import Control.Category


newtype Port m r a = Y { fromP :: Recipe m r a }

unit :: Applicative m => Port m r ()
unit = Y void

apply  :: Monad m => Recipe m a b -> Port m r a -> Port m r b
apply φ (Y f) = Y (φ . f)

split  :: Monad m => Port m r (a, b) -> (Port m r a, Port m r b)
split (Y f) = (Y (exl . f), Y (exr . f))
  -- NOTE: f gets duplicated here, and I think we don't want that?

merge  :: Monad m => (Port m r a, Port m r b) -> Port m r (a, b)
merge (Y f, Y g) = Y (f ▵ g)

recipe :: Monad m => (forall r. Port m r a -> Port m r b) -> Recipe m a b
recipe f = fromP (f (Y id))

(>>=) :: Monad m => Port m r a -> (Port m r a -> Port m r b) -> Port m r b
x >>= f = f x -- TODO: actually force execution of x

(>>) :: Monad m => Port m r a -> Port m r b -> Port m r b
Y x >> Y y = Y (exr . (x ▵ y))
