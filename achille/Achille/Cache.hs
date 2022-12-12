module Achille.Cache
  ( Cache
  , emptyCache
  , splitCache
  , joinCache
  , fromCache
  , toCache
  ) where

import GHC.Generics
import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)

import Data.Binary          qualified as Binary
import Data.ByteString.Lazy qualified as LBS

-- * Cache
--
-- $cache
-- All recipes are run with a local cache that they can use freely to remember
-- information between runs.

-- | The cache received by recipes. It is a list in order to make composition associative.
newtype Cache = Cache { chunks :: [ByteString] } deriving (Generic, Binary)

-- | The empty cache.
emptyCache :: Cache
emptyCache = Cache []

-- | Splits the cache in two.
splitCache :: Cache -> (Cache, Cache)
splitCache (Cache []) = (emptyCache, emptyCache)
splitCache (Cache (c:cs)) = (Binary.decode c, Cache cs)

-- | Combines two caches.
joinCache :: Cache -> Cache -> Cache
joinCache hd (Cache cs) = Cache (Binary.encode hd : cs)

-- | Retrieve a value from cache.
fromCache :: Binary a => Cache -> Maybe a
fromCache (Cache []) = Nothing
fromCache (Cache (c:cs)) =
  case Binary.decodeOrFail c of
    Left _ -> Nothing
    Right (_, _, x) -> Just x

-- | Writes a value to cache.
toCache :: Binary a => a -> Cache
toCache x = Cache [Binary.encode x]


