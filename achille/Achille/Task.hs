{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Achille.Task
  ( module Achille.Core.Task
  , pattern (:*:)
  , toURL
  , log
  , debug
  , write
  , copy
  , (-<.>)
  , readText
  , glob
  , match
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
  hiding (log, fst, snd, (>>), (>>=), fail, (.), reverse, take, drop, map)
import Control.Applicative (Applicative(liftA2))
import Control.Arrow (arr)
import Data.Map.Strict (Map)
import Data.Binary (Binary)
import System.FilePath.Glob (Pattern)
import Data.Text (Text)

import Achille.IO (AchilleIO)
import Achille.Writable (Writable)

import Achille.Recipe    qualified as Recipe

import Achille.Core.Task
import Achille.Path (Path)
import Achille.Path qualified as Path
import Data.List qualified as List

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

toURL :: Monad m => Task m Path -> Task m Text
toURL = apply Recipe.toURL

log :: (AchilleIO m, Monad m) => Task m Text -> Task m ()
log = apply Recipe.log

debug :: (AchilleIO m, Monad m) => Task m Text -> Task m ()
debug = apply Recipe.debug

write
  :: (AchilleIO m, Monad m, Writable m a)
  => Task m Path -> Task m a -> Task m Text
write path x = apply Recipe.write (path :*: x)

copy
  :: (AchilleIO m, Monad m)
  => Task m Path -> Task m Text
copy = apply Recipe.copy

(-<.>) :: Monad m => Task m Path -> Task m String -> Task m Path
path -<.> ext = liftA2 (Path.-<.>) path ext

readText :: (AchilleIO m, Monad m) => Task m Path -> Task m Text
readText = apply Recipe.readText


-- $lists
--
-- Usual operations on lists, lifted to tasks returning lists.
-- Each of them was implemented so that information change of input gets
-- propagated and preserved.

map :: Monad m => (a -> b) -> Task m [a] -> Task m [b]
map f = apply (Recipe.map f)

-- | Sort a list using the prelude @sort@.
reverse :: Monad m => Task m [a] -> Task m [a]
reverse = apply Recipe.reverse

-- | Sort a list using the prelude @sort@.
sort :: (Monad m, Ord a) => Task m [a] -> Task m [a]
sort = apply (arr List.sort)

-- | Sort a list using the prelude @sort@.
-- Crucially this takes care of tracking change information in the list.
sortOn :: (Monad m, Ord b) => (a -> b) -> Task m [a] -> Task m [a]
sortOn f = apply (arr $ List.sortOn f)

-- | Return the prefix of length @n@ of the input list.
take :: (Monad m) => Int -> Task m [a] -> Task m [a]
take n = apply (Recipe.take n)

-- | Drop the first @n@ elements of the input list.
drop :: Monad m => Int -> Task m [a] -> Task m [a]
drop n = apply (Recipe.drop n)

-- | Return all paths matching the given pattern.
glob :: (AchilleIO m, Monad m) => Task m Pattern -> Task m [Path]
glob = apply Recipe.glob

match
  :: (Monad m, AchilleIO m, Binary b, Eq b)
  => Task m Pattern -> (Task m Path -> Task m b) -> Task m [b]
match p f = for (glob p) \src -> cached (scoped src (f src))

-- $maps
--
-- Usual operations on ordered maps from keys to values, lifted to tasks.
-- Each of them was implemented so that information change of input gets
-- propagated and preserved.

infixl 9 !
(!) :: (Monad m, Ord k, AchilleIO m) => Task m (Map k a) -> Task m k -> Task m a
(!) m k = apply (Recipe.!) (m :*: k)

