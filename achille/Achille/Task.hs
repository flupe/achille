{-# LANGUAGE GADTs, RecordWildCards, ViewPatterns, PatternSynonyms #-}

module Achille.Task 
  ( module Achille.Core.Task
  , pattern (:*:)
  , toURL
  , write
  , copy
  , (-<.>)
  , readText
  , -- * Operations over lists
    --
    -- $lists
    map
  , reverse
  , sort
  , sortOn
  , take
  , drop
  , -- * Operations over maps
    --
    -- $maps
    (!)
  ) where

import Prelude
  hiding (fst, snd, (>>), (>>=), fail, (.), reverse, take, drop, map)
import Control.Applicative (Applicative(liftA2))
import Control.Category
import Control.Arrow (arr)
import Data.Binary (Binary)
import Data.Map.Strict (Map)
import Data.String (IsString(fromString))
import Data.Text (Text)
import System.FilePath.Glob (Pattern)

import Achille.Diffable (Value, value)
import Achille.IO (AchilleIO)
import Achille.Recipe (Recipe)
import Achille.Writable (Writable)

import Prelude           qualified
import Data.IntSet       qualified as IntSet
import System.FilePath   qualified as FilePath
import Achille.Recipe    qualified as Recipe
import Achille.Writable  qualified as Writable

import Achille.Core.Task
import Achille.Path (Path)
import Achille.Path qualified as Path

import Data.Binary.Instances.Time ()


-- | Pattern for destructuring and constructing /tuples/ of tasks.
--   It is intended to be used with @QualifiedDo@, as such:
--
-- > {-# LANGUAGE QualifiedDo #-}
-- > import Achille as A
-- >
-- > doStuff     :: Task IO (Text, Bool)
-- > doMoreStuff :: Task IO (Bool, Text) -> Task IO String
-- >
-- > rules :: Task IO String
-- > rules = A.do
-- >   x :*: y <- doStuff
-- >   doMoreStuff (y :*: x)

pattern (:*:) :: Task m a -> Task m b -> Task m (a, b)
pattern (:*:) x y <- (split -> (x, y))
  where (:*:) = pair

split :: Task m (a, b) -> (Task m a, Task m b)
split p = (fst p, snd p)

toURL :: Applicative m => Task m Path -> Task m Text
toURL = apply Recipe.toURL

write
  :: (AchilleIO m, Monad m, Writable m a)
  => Task m Path -> Task m a -> Task m Text
write path x = apply Recipe.write (path :*: x)

copy
  :: (AchilleIO m, Monad m)
  => Task m Path -> Task m Text
copy = apply Recipe.copy

(-<.>) :: Applicative m => Task m Path -> Task m String -> Task m Path
path -<.> ext = liftA2 (Path.-<.>) path ext

readText :: (AchilleIO m, Applicative m) => Task m Path -> Task m Text
readText = apply Recipe.readText


-- $lists
--
-- Usual operations on lists, lifted to tasks returning lists.
-- Each of them was implemented so that information change of input gets
-- propagated and preserved.

map :: Applicative m => (a -> b) -> Task m [a] -> Task m [b]
map f = apply (Recipe.map f)

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


-- $maps
--
-- Usual operations on ordered maps from keys to values, lifted to tasks.
-- Each of them was implemented so that information change of input gets
-- propagated and preserved.

infixl 9 !
(!) :: (Applicative m, Ord k) => Task m (Map k a) -> Task m k -> Task m a
(!) m k = apply (Recipe.!) (m :*: k)
