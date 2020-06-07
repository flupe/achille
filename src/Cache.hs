{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TupleSections #-}

module Cache
    ( Cache
    , CacheMatch
    , CacheWith
    , fromCache
    , toCache
    , emptyCache
    ) where

import Data.Binary         (Binary, decodeFile, encodeFile)
import Data.Maybe          (mapMaybe)
import Data.Functor        ((<&>))
import Data.Dynamic.Binary
import GHC.Generics

type Cache = Dynamic

emptyCache :: Cache
emptyCache = toDyn ()

type CacheMatch b  = [(FilePath, b)]
type CacheWith a b = (a, b)


fromCache :: (Typeable a, Binary a) => Cache -> Maybe a
fromCache = fromDynamic

toCache   :: (Typeable a, Binary a) => a     -> Cache
toCache = toDyn
