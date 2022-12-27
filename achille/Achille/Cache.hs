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
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

import Data.Binary          qualified as Binary
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as LBS

-- * Cache
--
-- $cache
-- All recipes are run with a local cache that they can use freely to remember
-- information between runs.

-- | The cache received by recipes.
newtype Cache = Cache ByteString deriving (Generic, Binary)

-- | The empty cache.
emptyCache :: Cache
emptyCache = Cache mempty

-- | Splits the cache in two.
splitCache :: Cache -> (Cache, Cache)
splitCache = fromMaybe (emptyCache, emptyCache) . fromCache

-- | Combines two caches into one.
joinCache :: Cache -> Cache -> Cache
joinCache a b = toCache (a, b)

-- NOTE:
--  @joinCache@ is /not/ associative
--  so we force @Seq@s to be right-nested when evaluating in Achille.Task
--  so that user-added parenthesis do not mess with the cache.

-- | Retrieve a value from cache.
fromCache :: Binary a => Cache -> Maybe a
fromCache (Cache c) =
  case Binary.decodeOrFail (LBS.fromStrict c) of
    Left _          -> Nothing
    Right (_, _, x) -> Just x

-- | Writes a value to cache.
toCache :: Binary a => a -> Cache
toCache = Cache . BS.toStrict . Binary.encode

