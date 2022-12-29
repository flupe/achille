{-# LANGUAGE GADTs, RecordWildCards, ViewPatterns, PatternSynonyms #-}

module Achille.Task 
  ( module Achille.Core.Task
  , pattern (:*:)
  , write
  , (-<.>)
  , -- * Operations over lists
    --
    -- $lists
    reverse
  , sort
  , sortOn
  , take
  , drop
  ) where

import Prelude hiding (fst, snd, (>>), (>>=), fail, (.), reverse, take, drop)
import Control.Applicative (Applicative(liftA2))
import Control.Category
import Control.Arrow (arr)
import Data.Binary (Binary)
import Data.String (IsString(fromString))
import System.FilePath.Glob (Pattern)

import Achille.Diffable (Value, value)
import Achille.IO (AchilleIO)
import Achille.Recipe (Recipe)
import Achille.Writable (Writable)

import Prelude           qualified as Prelude
import Data.IntSet       qualified as IntSet
import System.FilePath   qualified as FilePath
import Achille.Recipe    qualified as Recipe
import Achille.Writable  qualified as Writable

import Achille.Core.Task

import Data.Binary.Instances.Time ()


instance Applicative m => Functor (Task m) where
  fmap f x = apply (arr f) x
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Task m) where
  -- NOTE(flupe): values lifted with @pure@ are considered to always be old.
  --              maybe we want to make them always new, but a choice has to be made.
  --              over or under approximating incrementality.
  pure x = val (value x False)
  {-# INLINE pure #-}

  liftA2 f x y = apply (arr (uncurry f)) (pair x y)
  {-# INLINE liftA2 #-}

instance (Applicative m, IsString a) => IsString (Task m a) where
  fromString = pure . fromString
  {-# INLINE fromString #-}


-- | Pattern for destructuring and constructing /tuples/ of tasks.
--   It is intended to be used with @QualifiedDo@, as such:
--
-- > {-# LANGUAGE QualifiedDo #-}
-- > import Achille as A
-- >
-- > doStuff     :: Program IO (Text, Bool)
-- > doMoreStuff :: Program IO (Bool, Text) -> Program IO String
-- >
-- > rules :: Program IO String
-- > rules = A.do
-- >   x :*: y <- doStuff
-- >   doMoreStuff (y :*: x)

pattern (:*:) :: Task m a -> Task m b -> Task m (a, b)
pattern (:*:) x y <- (split -> (x, y))
  where (:*:) = pair

split :: Task m (a, b) -> (Task m a, Task m b)
split p = (fst p, snd p)

write
  :: (AchilleIO m, Monad m, Writable m a)
  => Task m FilePath -> Task m a -> Task m FilePath
write path x = apply Recipe.write (path :*: x)

(-<.>) :: Applicative m => Task m FilePath -> Task m FilePath -> Task m FilePath
path -<.> ext = liftA2 (FilePath.-<.>) path ext


-- $lists
--
-- Usual operations on lists, lifted to tasks returning lists.
-- Each of them was implemented so that information change of input gets
-- propagated and preserved.

-- | Sort a list using the prelude @sort@.
reverse :: Applicative m => Task m [a] -> Task m [a]
reverse = apply Recipe.reverse

-- | Sort a list using the prelude @sort@.
sort :: (Applicative m, Ord a) => Task m [a] -> Task m [a]
sort = apply Recipe.sort

-- | Sort a list using the prelude @sort@.
-- Crucially this takes care of tracking change information in the list.
sortOn :: (Applicative m, Ord b) => (a -> b) -> Task m [a] -> Task m [a]
sortOn f = apply (Recipe.sortOn f)

-- | Return the prefix of length @n@ of the input list.
take :: (Applicative m) => Int -> Task m [a] -> Task m [a]
take n = apply (Recipe.take n)

-- | Drop the first @n@ elements of the input list.
drop :: Applicative m => Int -> Task m [a] -> Task m [a]
drop n = apply (Recipe.drop n)
