{-# LANGUAGE ApplicativeDo, NamedFieldPuns #-}
module Achille.Recipe
  ( Context(..)
  , Recipe(..)
  , readText
  )
  where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Achille.Cache
import Achille.Diffable (Value, value, hasChanged, joinPair)
import Achille.IO as AIO


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


-- TODO(flupe): actually bring back a GADT also for Recipe m
--              to make it a cartesian category again
--              and define my own proc notation
instance Monad m => Category (Recipe m) where
  id = Recipe
    { recipeName = "id"
    , runRecipe = \ctx cache v -> pure (v, cache)
    }

  g . f = Recipe
    { recipeName = recipeName g <> " . " <> recipeName f
    , runRecipe = \ctx cache v -> do
        let (cf, cg) = splitCache cache
        (v', cf) <- runRecipe f ctx cf v
        (vz, cg) <- runRecipe g ctx cf v'
        pure (vz, joinCache cf cg)
    }

instance Monad m => Arrow (Recipe m) where
  arr    = undefined
  first  = undefined
  second = undefined
  f &&& g = Recipe
    { recipeName = recipeName f <> " &&& " <> recipeName g
    , runRecipe = \ctx cache v -> do
        let (cf, cg) = splitCache cache
        (vx, cf) <- runRecipe f ctx cf v
        (vy, cg) <- runRecipe g ctx cg v
        pure (joinPair vx vy, joinCache cf cg)
    }

-- | Read text from file.
readText :: (Applicative m, AchilleIO m) => Recipe m FilePath Text
readText = Recipe 
  { recipeName = "readText"
  , runRecipe = \Context{lastTime} cache v@(src, _) -> do
      mtime <- getModificationTime src
      txt   <- decodeUtf8 <$> AIO.readFile src
      pure (value txt (hasChanged v || mtime > lastTime), cache)
  }
