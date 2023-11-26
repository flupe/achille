{-# LANGUAGE UndecidableInstances, DerivingStrategies, ScopedTypeVariables #-}
module Achille.Cache
  ( Cache
  , emptyCache
  , splitCache
  , joinCache
  , fromCache
  , toCache
  , defCaches
  ) where

import GHC.Generics
import Data.Binary (Binary(get, put), Get)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Generics.SOP (NP(..), NS(..), All, Compose)

import Data.Binary          qualified as Binary
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Generics.SOP         qualified as SOP

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


-- TODO: move this somewhere else
instance (Binary a, All SOP.Top xs) => Binary (NP (SOP.K a) xs) where
  put Nil       = pure ()
  put (x :* xs) = put x *> put xs

  get :: Get (NP (SOP.K a) xs)
  get = case SOP.sList :: SOP.SList xs of
    SOP.SNil  -> pure Nil
    SOP.SCons -> (:*) <$> get <*> get

deriving newtype instance Binary a => Binary (SOP.K a b)

defCaches :: forall xs. All SOP.Top xs => NP (SOP.K Cache) xs
defCaches = case SOP.sList :: SOP.SList xs of 
  SOP.SNil  -> Nil
  SOP.SCons -> SOP.K emptyCache :* defCaches
