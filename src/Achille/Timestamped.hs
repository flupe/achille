{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Achille.Timestamped
    ( Timestamped
    , IsTimestamped
    , timestamp
    , timestamped
    , timestampedWith
    , compareTimestamped
    , recentFirst
    , oldFirst
    ) where

import Data.Ord           (Ord, compare, Ordering)
import System.FilePath    (FilePath)
import Data.Dates         (DateTime(..))
import Data.List          (sortBy, sort)
import Data.Typeable      (Typeable)
import Data.Binary        (Binary, put, get)
import Data.Dates.Formats (parseDateFormat)
import System.FilePath    (takeFileName)

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
      Left _  -> mempty


timestamped :: IsTimestamped a => a -> Timestamped a
timestamped x = Timestamped (timestamp x) x

timestampedWith :: (a -> DateTime) -> a -> Timestamped a
timestampedWith f x = Timestamped (f x) x

compareTimestamped :: IsTimestamped a => a -> a -> Ordering
compareTimestamped x y = compare (timestamp x) (timestamp y)

recentFirst :: IsTimestamped a => [a] -> [a]
recentFirst = sortBy (flip compareTimestamped)

oldFirst :: IsTimestamped a => [a] -> [a]
oldFirst = sortBy compareTimestamped
