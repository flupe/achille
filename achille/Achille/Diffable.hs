{-# LANGUAGE TypeFamilyDependencies #-}
module Achille.Diffable
  ( Value(..)
  , value
  , unit
  , Diffable(..)

  , ListChange
  , takeChanges
  , sortChangesOn
  , dropChanges
  , listChangeVal
  ) where

import Data.Foldable (Foldable(..), foldMap)
import Data.List (sortOn)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as SMap
import Data.Monoid (Any(..))

-- | Wrapper containing a value of type @a@ and information about 
--   how it has changed since the last run.
data Value a = Value
  { theVal     :: a
  , hasChanged :: Bool
  , changeInfo :: Maybe (ChangeInfo a)
  }

instance Functor Value where
  -- for any f, we can only be conservative and assume it is injective
  -- if the input changes, we consider the output to also change and be "dirty"
  -- in general, we cannot give more interesting information about the change
  fmap f (Value x c _) = value c (f x)

value
  :: Bool -- ^ Whether the value has changed sinc the last run.
  -> a    -- ^ The value.
  -> Value a
value c x = Value x c Nothing

-- | The unit value, that never changes.
unit :: Value ()
unit = value False ()


-- | Typeclass for things that carry more information about change between runs.
class Diffable a where
  type ChangeInfo a = r | r -> a

  splitValue :: Value a -> ChangeInfo a

  joinValue  :: ChangeInfo a -> Value a


instance Diffable (a, b) where
  type ChangeInfo (a, b) = (Value a, Value b)

  splitValue :: Value (a, b) -> (Value a, Value b)
  splitValue (Value (x, y) c Nothing) = (value c x, value c y)
  splitValue (Value _ c (Just vs)) = vs

  joinValue :: (Value a, Value b) -> Value (a, b)
  joinValue c@(x, y) =
    Value (theVal x, theVal y)
          (hasChanged x || hasChanged y)
          (Just c)

-- Lists
--

data ListChange a
  = Deleted a
  | Inserted a
  | Kept (Value a)
  deriving (Functor)

listChangeVal :: ListChange a -> a
listChangeVal (Deleted x) = x
listChangeVal (Inserted x) = x
listChangeVal (Kept v) = theVal v

listChangeToVal :: ListChange a -> Maybe a
listChangeToVal (Deleted _) = Nothing
listChangeToVal (Inserted x) = Just x
listChangeToVal (Kept v) = Just (theVal v)

listChangeDidChange :: ListChange a -> Bool
listChangeDidChange (Deleted _) = True
listChangeDidChange (Inserted _) = True
listChangeDidChange (Kept v) = hasChanged v

sortChangesOn :: Ord b => (a -> b) -> [ListChange a] -> [ListChange a]
sortChangesOn f = sortOn (f . listChangeVal)

flattenChanges :: [ListChange a] -> [a]
flattenChanges = mapMaybe listChangeToVal

takeChanges :: Int -> [ListChange a] -> [ListChange a]
takeChanges n _ | n <= 0 = []
takeChanges n (c:cs) =
  let n' = case c of
             Deleted _ -> n
             _         -> n - 1
  in c : takeChanges n' cs

dropChanges :: Int -> [ListChange a] -> [ListChange a]
dropChanges n cs | n <= 0 = cs
dropChanges n (c:cs) =
  let n' = case c of
             Deleted _  -> n
             _          -> n - 1
  in dropChanges n' cs

instance Diffable [a] where
  type ChangeInfo [a] = [ListChange a]

  splitValue :: Value [a] -> [ListChange a]
  splitValue (Value xs c Nothing) = [ Kept (value c x) | x <- xs ]
  splitValue (Value x c (Just cs)) = cs

  joinValue :: [ListChange a] -> Value [a]
  joinValue cs =
    Value (flattenChanges cs)
          (any listChangeDidChange cs)
          (Just cs)


-- The following instance is incorrect and should be made like the previous one

instance Ord k => Diffable (Map k v) where
  type ChangeInfo (Map k v) = Map k (Value v)

  splitValue :: Value (Map k v) -> Map k (Value v)
  splitValue (Value m c Nothing) = value c <$> m
  splitValue (Value _ _ (Just mv)) = mv
  
  joinValue :: Map k (Value v) -> Value (Map k v)
  joinValue mv = Value (theVal <$> mv) (getAny (foldMap (Any . hasChanged) mv)) (Just mv)
