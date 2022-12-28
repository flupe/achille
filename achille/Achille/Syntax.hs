{-# LANGUAGE Rank2Types, PatternSynonyms, ViewPatterns, FlexibleInstances, IncoherentInstances #-}
{- | Module      : Achille.Syntax
     Description : The EDSL syntax exposed to the user
     Copyright   : (c) flupe, 2022
     License     : MIT
     Maintainer  : lucas@escot.me

     This module exports the syntax available to the user when writing achille programs.
-}
module Achille.Syntax
  ( Program
  , Achille(..)
  , pattern (:*:)
  , write
  , (-<.>)
  ) where

import Prelude hiding (fst, snd, (>>))
import Control.Applicative (Applicative(liftA2))
import Control.Arrow (arr)
import Data.Binary (Binary)
import Data.String (IsString(fromString))
import System.FilePath.Glob (Pattern)

import Achille.Diffable (Value, value)
import Achille.IO (AchilleIO)
import Achille.Recipe (Recipe)
import Achille.Writable (Writable)

import System.FilePath  qualified as FilePath
import Achille.Recipe   qualified as Recipe
import Achille.Writable qualified as Writable


-- | A program is a task definition, polymorphic over the kind of underlying task family.
type Program m a = forall task. Achille task => task m a


-- | Interface for all the operations supported by achille programs.
class Achille (task :: (* -> *) -> * -> *) where
  -- | Sequence two tasks, ignoring the result of the first.
  (>>) :: task m a -> task m b -> task m b

  -- | Sequence two tasks, making the result of the first available to the second.
  --   This enables binding variables using @QualifiedDo@, while preventing duplication.
  (>>=) :: task m a -> (task m a -> task m b) -> task m b

  -- | Make a task out of a pair of tasks.
  pair :: task m a -> task m b -> task m (a, b)

  -- | Fail with an error message.
  fail :: String -> task m a

  -- | For every path matching the Glob pattern, run the given task and
  --   collect all results in a list.
  --   @match@ caches the list, and only triggers the task on a given path if
  --   the underlying file is new or has changed since the last run.
  match :: Binary b => Pattern -> (task m FilePath -> task m b) -> task m [b]

  -- | For every path matching the Glob pattern, run the given task.
  --   @match_@ only triggers the task on a given path if the underlying file is
  --   new or has changed since the last run.
  match_ :: Pattern -> (task m FilePath -> task m b) -> task m ()

  val :: Value a -> task m a

  -- | Make a task out of a recipe applied to a task.
  apply  :: Recipe m a b -> task m a -> task m b


instance (Applicative m, Achille task) => Functor (task m) where
  fmap f = apply (arr f)

instance (Applicative m, Achille task) => Applicative (task m) where
  pure x = val (value x False)
  (*>) = (>>)
  (<*) x y = y >> x
  liftA2 f x y = apply (arr $ uncurry f) (pair x y)

-- TODO(flupe): cache value and check change
instance (IsString a, Achille task) => IsString (task m a) where
  fromString x = val (value (fromString x) False)

fst :: Achille task => task m (a, b) -> task m a
fst = apply Recipe.exl

snd :: Achille task => task m (a, b) -> task m b
snd = apply Recipe.exr

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

pattern (:*:) :: Achille task => task m a -> task m b -> task m (a, b)
pattern (:*:) x y <- (split -> (x, y))
  where (:*:) = pair

split :: Achille task => task m (a, b) -> (task m a, task m b)
split p = (fst p, snd p)

write
  :: (Achille task, AchilleIO m, Monad m, Writable m a)
  => task m FilePath -> task m a -> task m FilePath
write path x = apply Recipe.write (path :*: x)

(-<.>)
  :: (Applicative m, Achille task) 
  => task m FilePath -> task m FilePath -> task m FilePath
path -<.> ext = liftA2 (FilePath.-<.>) path ext
