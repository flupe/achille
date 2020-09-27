{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

-- | Defines utilies for working with timestamped things.
module Achille.Timestamped
    ( Timestamped(..)
    , IsTimestamped
    , timestamp
    , timestamped
    , timestampedWith
    , compareTimestamped
    , recentFirst
    , oldFirst
    ) where

import Data.Ord                   (Ord, compare, Ordering)
import System.FilePath            (FilePath)
import Data.List                  (sortBy, sort)
import Data.Typeable              (Typeable)
import Data.Binary                (Binary, put, get)
import Data.Time.Calendar         (fromGregorian)
import Data.Time                  (UTCTime(..), secondsToDiffTime)
import Data.Time.Format           (readSTime, defaultTimeLocale)
import System.FilePath            (takeFileName)
import Data.Binary.Instances.Time ()

-- | Container for timestamping data.
data Timestamped a = Timestamped UTCTime a
    deriving (Show, Eq, Ord, Typeable, Functor)

instance Binary a => Binary (Timestamped a) where
    put (Timestamped d x) = put d >> put x
    get                   = Timestamped <$> get <*> get


-- | Class for values that can be timestamped.
class IsTimestamped a where
    -- | Retrieve a datetime from a value.
    timestamp :: a -> UTCTime

instance IsTimestamped (Timestamped a) where
    timestamp (Timestamped d _) = d

instance IsTimestamped FilePath where
    timestamp p =
        case readSTime False defaultTimeLocale "%Y-%m-%d" (takeFileName p) of
            [(t, _)] -> t
            _        -> UTCTime (fromGregorian 1970 01 01) (secondsToDiffTime 0)


-- | Wrap a value that can be timestamped.
timestamped :: IsTimestamped a => a -> Timestamped a
timestamped x = Timestamped (timestamp x) x

-- | Wrap a value that can be timestamped, using the given function for
--   retrieving the timestamp.
timestampedWith :: (a -> UTCTime) -> a -> Timestamped a
timestampedWith f x = Timestamped (f x) x

-- | Compare two timestamped values.
compareTimestamped :: IsTimestamped a => a -> a -> Ordering
compareTimestamped x y = compare (timestamp x) (timestamp y)

-- | Sort timestamped values from most recent to oldest.
recentFirst :: IsTimestamped a => [a] -> [a]
recentFirst = sortBy (flip compareTimestamped)

-- | Sort timestamped values from oldest to most recent.
oldFirst :: IsTimestamped a => [a] -> [a]
oldFirst = sortBy compareTimestamped
