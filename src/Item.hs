module Item where

import Data.Dates (DateTime)
import Data.List  (sortBy)

data Item a = Item
    { itemDate  :: DateTime
    , itemValue :: a
    } deriving (Eq, Show)

instance Eq a => Ord (Item a) where
    compare (Item d1 _) (Item d2 _) = compare d1 d2

recentFirst :: Eq a => [Item a] -> [Item a]
recentFirst = sortBy (flip compare)

