{-# LANGUAGE InstanceSigs, DeriveGeneric, DeriveAnyClass #-}

-- | Core recipe definition.
module Achille.Recipe where

import Control.Category
import Data.Map
import Data.Binary
import Data.ByteString.Lazy as LBS
import GHC.Generics
import qualified Data.Binary as Binary

-- | The cache received by recipes. It is a list in order to make composition associative.
newtype Cache = Cache { chunks :: [ByteString] } deriving (Generic, Binary)

-- | Context in which a recipe is run.
data Context = Context
  { tagged :: Map String Cache -- ^ A map of caches, used for expensive computations that 
                               -- should be made insensitive to code refactoring
  }

-- | The recipe abstraction.
data Recipe m a b = Recipe { runRecipe :: Context -> Cache -> a -> m (b, Cache) }


emptyCache :: Cache
emptyCache = Cache []


-- | Splits the cache in two.
splitCache :: Cache -> (Cache, Cache)
splitCache (Cache []) = (emptyCache, emptyCache)
splitCache (Cache (c:cs)) = (Binary.decode c, Cache cs)


-- | Combines two caches.
joinCache :: Cache -> Cache -> Cache
joinCache hd (Cache cs) = Cache (Binary.encode hd : cs)


instance Monad m => Category (Recipe m) where
  id :: Recipe m a a
  id = Recipe \ctx cache x -> pure (x, cache)

  (.) :: Recipe m b c -> Recipe m a b -> Recipe m a c
  Recipe g . Recipe f = Recipe \ctx cache x -> do
    let (cf, cg) = splitCache cache
    (y, cf') <- f ctx cf x
    (z, cg') <- g ctx cg y
    pure (z, joinCache cf' cg')
