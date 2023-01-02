module Achille.Core.Task
  ( Task
  , runTask
  , (>>)
  , (>>=)
  , pair
  , fst
  , snd
  , fail
  , match
  , match_
  , apply
  , val
  , toProgram
  ) where

import Prelude hiding ((.), id, seq, fail, (>>=), (>>), fst, snd)

import Control.Category
import Control.Monad (forM)
import Control.Applicative (Alternative, empty, liftA2)
import Control.Arrow

import Data.Binary (Binary)
import Data.Functor ((<&>))
import Data.IntMap.Strict (IntMap, (!?))
import Data.IntSet (IntSet)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Data.Time (UTCTime)

import System.FilePath ((</>), makeRelative)
import System.FilePath.Glob (Pattern)
import Unsafe.Coerce (unsafeCoerce)

import Prelude            qualified as Prelude
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet        qualified as IntSet
import Data.Map.Strict    qualified as Map

import Achille.Cache
import Achille.Diffable
import Achille.IO

import Achille.Core.Recipe
import Achille.Core.Program


-- NOTE(flupe): maybe we should *NOT* make all the applications strict.
--              it's likely that's all that is required to enable recursive tasks.

-- | Core abstraction for build tasks.
newtype Task m a = T { unTask :: Int -> (Program m a, IntSet) }

instance Applicative m => Functor (Task m) where
  fmap f x = apply (arr f) x
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Task m) where
  -- NOTE(flupe): values lifted with @pure@ are considered to always be old.
  --              maybe we want to make them always new, but a choice has to be made.
  --              over or under approximating incrementality.
  pure x = val (value False x)
  {-# INLINE pure #-}

  liftA2 f x y = apply (arr (uncurry f)) (pair x y)
  {-# INLINE liftA2 #-}

instance (Applicative m, IsString a) => IsString (Task m a) where
  fromString = pure . fromString
  {-# INLINE fromString #-}


toProgram :: Task m a -> Program m a
toProgram t = Prelude.fst $! unTask t 0
{-# INLINE toProgram #-}

runTask
  :: (Monad m, MonadFail m, AchilleIO m)
  => Task m a -> Context -> Cache -> m (Value a, Cache)
runTask (toProgram -> p) = runProgram p
{-# INLINE runTask #-}

-- | Sequence two tasks, ignoring the result of the first.
(>>) :: Task m a -> Task m b -> Task m b
T x >> T y = T \n ->
  let (x', vsx) = x $! n
      (y', vsy) = y $! n
  in (seq x' y', vsx <> vsy)
 where seq :: Program m a -> Program m b -> Program m b
       seq (Seq x y) z = x `seq` (y `seq` z)
       seq x y = Seq x y
       {-# INLINE seq #-}
{-# INLINE (>>) #-}

-- | Sequence two tasks, making the result of the first available to the second.
--   This enables binding variables using @QualifiedDo@, while preventing duplication.
(>>=) :: Task m a -> (Task m a -> Task m b) -> Task m b
(>>=) (T x) f = T \n ->
  let (x', vsx) = x $! n
      (f', vsf) = unTask (f $ T \_ -> (Var n, IntSet.singleton n)) $! n + 1
  in (Bind x' f', vsx <> vsf)
{-# INLINE (>>=) #-}

-- | Make a Task out of a pair of Tasks.
pair :: Task m a -> Task m b -> Task m (a, b)
pair (T x) (T y) = T \n ->
  let (x', vsx) = x $! n
      (y', vsy) = y $! n
  in (Pair x' y', vsx <> vsy)
{-# INLINE pair #-}

fst :: Task m (a, b) -> Task m a
fst = apply Exl

snd :: Task m (a, b) -> Task m b
snd = apply Exr

-- | Fail with an error message.
fail :: String -> Task m a
fail s = T \_ -> (Fail s, IntSet.empty)
{-# INLINE fail #-}

-- | For every path matching the Glob pattern, run the given Task and
--   collect all results in a list.
--   @match@ caches the list, and only triggers the Task on a given path if
--   the underlying file is new or has changed since the last run.
match :: (Binary b, Eq b) => Pattern -> (Task m FilePath -> Task m b) -> Task m [b]
match pat t = T \n ->
  -- remove locally-bound variables
  -- NOTE(flupe): maybe we can pospone the filtering at evaluation
  --              because maybe it gets in the way of compile-time translation
  let (t', IntSet.filter (< n) -> vst) = unTask (t $ T \_ -> (Var n, IntSet.empty)) $! n + 1
  in (Match pat t' vst, vst)
{-# INLINE match #-}

-- | For every path matching the Glob pattern, run the given Task.
--   @match_@ only triggers the Task on a given path if the underlying file is
--   new or has changed since the last run.
match_ :: Pattern -> (Task m FilePath -> Task m b) -> Task m ()
match_ pat t = T \n ->
  -- remove locally-bound variables
  let (t', IntSet.filter (< n) -> vst) = unTask (t $ T \_ -> (Var n, IntSet.empty)) $! n + 1
  in (Match_ pat t' vst, vst)
{-# INLINE match_ #-}

-- | Lift a value into a task.
val :: Value a -> Task m a
val !x = T \_ -> (Val x, IntSet.empty)
{-# INLINE val #-}

-- | Make a Task out of a recipe applied to a Task.
apply  :: Recipe m a b -> Task m a -> Task m b
apply !r (T x) = T \n ->
  let (x', vsx) = x $! n in (app r x', vsx)
    where app :: Recipe m a b -> Program m a -> Program m b
          app r (Apply s x) = app (r . s) x
          app Id x = x
          app r x = Apply r x
{-# INLINE apply #-}
