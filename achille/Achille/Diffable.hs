{-# LANGUAGE TypeFamilyDependencies #-}
module Achille.Diffable
  ( Value(..)
  , value
  , unit
  , Diffable(..)

  , ListChange(..)
  , takeListChanges
  , dropListChanges
  , listChangeToVal
  , mapZipChanges
  , cmpChangesAsc
  ) where

import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
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
  splitValue (Value _ _ (Just vs)) = vs

  joinValue :: (Value a, Value b) -> Value (a, b)
  joinValue c@(x, y) =
    Value (theVal x, theVal y)
          (hasChanged x || hasChanged y)
          (Just c)

-- Lists
--

-- | List containing information about how it changed.
data ListChange a
  = Deleted
    -- ^ an element at this position was deleted
  | Inserted a
    -- ^ an element at this position was inserted
  | Kept (Value a)
    -- ^ an element here has been kept, but may have changed.
  deriving (Functor)

listChangeToVal :: ListChange a -> Maybe a
listChangeToVal Deleted      = Nothing
listChangeToVal (Inserted x) = Just x
listChangeToVal (Kept v)     = Just (theVal v)

listChangeDidChange :: ListChange a -> Bool
listChangeDidChange Deleted      = True
listChangeDidChange (Inserted _) = True
listChangeDidChange (Kept v)     = hasChanged v

-- NOTE(flupe): since we don't store deleted values, we can't really ALWAYS correctly sort changes
-- sortChangesOn :: Ord b => (a -> b) -> [ListChange a] -> [ListChange a]
-- sortChangesOn f = sortOn (f . listChangeVal)

flattenChanges :: [ListChange a] -> [a]
flattenChanges = mapMaybe listChangeToVal

takeListChanges :: Int -> [ListChange a] -> [ListChange a]
takeListChanges _ [] = []
takeListChanges !n _ | n <= 0 = []
takeListChanges !n (c:cs) =
  let n' = case c of
             Deleted -> n
             _       -> n - 1
  in c : takeListChanges n' cs

mapZipChanges :: Monad m => [ListChange a] -> [b] -> b -> (Value a -> b -> m c) -> m [c]
mapZipChanges []              _      _ _   = pure []
mapZipChanges (Deleted   :cs) bs     d f = mapZipChanges cs (drop 1 bs) d f
mapZipChanges (Inserted x:cs) bs     d f = (:) <$> f (value True x) d <*> mapZipChanges cs bs d f
mapZipChanges (Kept     v:cs) (b:bs) d f = (:) <$> f v b <*> mapZipChanges cs bs d f
mapZipChanges (Kept     v:cs) []     d f = (:) <$> f v d <*> mapZipChanges cs [] d f

cmpChangesAsc :: Ord a => [a] -> [a] -> [ListChange a]
cmpChangesAsc [] [] = []
cmpChangesAsc xs [] = [ Deleted | _ <- xs ]
cmpChangesAsc [] ys = map Inserted ys
cmpChangesAsc (x:xs) (y:ys) =
  case compare x y of
    EQ -> Kept (value False x) : cmpChangesAsc xs     ys
    LT -> Deleted              : cmpChangesAsc xs     (y:ys)
    GT -> Inserted y           : cmpChangesAsc (x:xs) (y:ys)

dropListChanges :: Int -> [ListChange a] -> [ListChange a]
dropListChanges _ [] = []
dropListChanges !n cs | n <= 0 = cs
dropListChanges !n (c:cs) =
  let n' = case c of
             Deleted  -> n
             _        -> n - 1
  in dropListChanges n' cs

instance Diffable [a] where
  type ChangeInfo [a] = [ListChange a]

  splitValue :: Value [a] -> [ListChange a]
  splitValue (Value xs c Nothing) = [ Kept (value c x) | x <- xs ]
  splitValue (Value _ _ (Just cs)) = cs

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
