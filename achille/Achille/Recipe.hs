module Achille.Recipe
  ( Context(..)
  , Recipe(..)
  )
  where

import Data.Time (UTCTime)

import Achille.Cache
import Achille.Diffable (Value)

-- | Context in which tasks and recipes are run.
data Context = Context
  { lastTime :: UTCTime -- ^ Time of the last run.
  }

-- | A recipe is a glorified Kleisli arrow, computing a value of type @b@ in some monad @m@
--   given some input of type @a@ and a context. The recipe has access to a local cache,
--   preserved between runs.

data Recipe m a b = Recipe
  { recipeName :: String
  , runRecipe  :: Context -> Cache -> Value a -> m (Value b, Cache) 
  }
