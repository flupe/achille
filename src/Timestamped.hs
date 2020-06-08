{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Timestamped where

import Data.Ord
import System.FilePath (FilePath)
import Data.Dates (DateTime(..))
import Data.List  (sortBy, sort)
import Data.Typeable (Typeable)
import Data.Binary (Binary, put, get)
import Data.Time.Clock
import Data.Dates.Formats (parseDateFormat)
import System.FilePath (takeFileName)

-- | Container for timestamping data
data Timestamped a = Timestamped DateTime a
    deriving (Show, Eq, Ord, Typeable, Functor)

instance Binary DateTime where
    put (DateTime y m d hr mi sc) = put y  >> put m  >> put d
                                 >> put hr >> put mi >> put sc
    get = DateTime <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary a => Binary (Timestamped a) where
    put (Timestamped d x) = put d >> put x
    get                   = Timestamped <$> get <*> get


-- | Class for values that can be timestamped
class IsTimestamped a where
    timestamp :: a -> DateTime

instance IsTimestamped (Timestamped a) where
    timestamp (Timestamped d _) = d

instance IsTimestamped FilePath where
    timestamp p = case parseDateFormat "YYYY-MM-DD" (takeFileName p) of
      Right d -> d


timestamped :: IsTimestamped a => a -> Timestamped a
timestamped x = Timestamped (timestamp x) x

timestampedWith :: (a -> DateTime) -> a -> Timestamped a
timestampedWith f x = Timestamped (f x) x

recentFirst :: (Eq a, Ord a) => [Timestamped a] -> [Timestamped a]
recentFirst = sortBy (flip compare)

oldFirst :: (Eq a, Ord a) => [Timestamped a] -> [Timestamped a]
oldFirst = sort
