{-# LANGUAGE DefaultSignatures, TypeFamilyDependencies #-}
module Achille.Diffable
  ( Value(..)
  , value
  , unit
  , Diffable(..)
  ) where

import Data.Foldable (Foldable(..), foldMap)
import Data.Bifunctor (bimap)
import Data.Bifoldable (biany)
import Data.Monoid (Any(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as SMap

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
  -- in general, we cannot give more information about the change
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


instance Diffable [a] where
  type ChangeInfo [a] = [Value a]

  splitValue :: Value [a] -> [Value a]
  splitValue (Value xs c Nothing) = map (value c) xs
  splitValue (Value _ _ (Just vs)) = vs

  joinValue :: [Value a] -> Value [a]
  joinValue vs = Value (map theVal vs) (any hasChanged vs) (Just vs)


instance Diffable (Either a b) where
  type ChangeInfo (Either a b) = Either (Value a) (Value b)

  splitValue :: Value (Either a b) -> Either (Value a) (Value b)
  splitValue (Value e c Nothing) = bimap (value c) (value c) e
  splitValue (Value _ _ (Just vs)) = vs

  joinValue :: Either (Value a) (Value b) -> Value (Either a b)
  joinValue vs =
    Value (bimap theVal theVal vs)
          (biany hasChanged hasChanged vs) 
          (Just vs)


instance Ord k => Diffable (Map k v) where
  type ChangeInfo (Map k v) = Map k (Value v)

  splitValue :: Value (Map k v) -> Map k (Value v)
  splitValue (Value m c Nothing) = value c <$> m
  splitValue (Value _ _ (Just mv)) = mv
  
  joinValue :: Map k (Value v) -> Value (Map k v)
  joinValue mv = Value (theVal <$> mv) (getAny (foldMap (Any . hasChanged) mv)) (Just mv)
