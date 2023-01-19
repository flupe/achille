{-# LANGUAGE OverloadedStrings #-}
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
  , apply
  , val
  , void
  , toProgram
  ) where

import Prelude hiding ((.), id, seq, fail, (>>=), (>>), fst, snd)

import Control.Category
import Control.Monad (forM)
import Control.Monad.Reader.Class
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

import System.FilePath.Glob (Pattern)
import System.FilePath qualified as FP
import Unsafe.Coerce (unsafeCoerce)

import Prelude            qualified
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet        qualified as IntSet
import Data.Map.Strict    qualified as Map

import Achille.Cache
import Achille.Context (Context)
import Achille.Diffable as Diffable
import Achille.DynDeps (DynDeps)
import Achille.Path
import Achille.Result
import Achille.IO

import Achille.Core.Recipe
import Achille.Core.Program

import Achille.Context qualified as Ctx


-- NOTE(flupe): maybe we should *NOT* make all the applications strict.
--              it's likely that's all that is required to enable recursive tasks.

-- | Core abstraction for build tasks.
newtype Task m a = T { unTask :: Int -> (Program m a, IntSet) }

instance Applicative m => Functor (Task m) where
  fmap f = apply (arr f)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Task m) where
  -- NOTE(flupe): values lifted with @pure@ are considered to always be old.
  --              maybe we want to make them always new, but a choice has to be made.
  --              over or under approximating incrementality.
  pure x = val (value False x)
  {-# INLINE pure #-}

  liftA2 f x y = apply (arr (uncurry f)) (pair x y)
  {-# INLINE liftA2 #-}

instance {-# OVERLAPPABLE #-} (Applicative m, IsString a) => IsString (Task m a) where
  fromString = pure . fromString
  {-# INLINE fromString #-}

-- NOTE(flupe): Paths and Patterns are not just lifted to tasks, but also get the current
--              directory prepended.

instance {-# OVERLAPPING #-} Monad m => IsString (Task m Path) where
  fromString p = apply rec (pure ())
    where
      rec :: Recipe m () Path
      rec = recipe "Achille.Core.Task.toPath" \cache _ -> do
        curr <- reader Ctx.currentDir
        let path :: Path = normalise (curr </> fromString p)
        let same = fromCache cache == Just path
        pure (value (not same) path, toCache path)

-- instance {-# OVERLAPPING #-} Applicative m => IsString (Task m Pattern) where
--   fromString p = apply rec (pure ())
--     where
--       rec :: Recipe m () Pattern
--       rec = recipe "Achille.Core.Task.toPath" \Context{..} cache _ -> do
--         let path :: Pattern = fromString (toFilePath currentDir FP.</> p)
--         pure (value False path, cache)


toProgram :: Task m a -> Program m a
toProgram t = Prelude.fst $! unTask t 0
{-# INLINE toProgram #-}

runTask
  :: (Monad m, MonadFail m, AchilleIO m)
  => Task m a -> Context -> Cache -> m (Value a, DynDeps, Cache)
runTask (toProgram -> p) ctx cache = do
  ((v, c), deps) <- runResult (runProgram p cache) ctx
  pure (v, deps, c)
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
      (f', vsf) = unTask (f $ T $ const (Var n, IntSet.singleton n)) $! n + 1
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

void :: Task m a -> Task m ()
void = apply Void

-- | Fail with an error message.
fail :: String -> Task m a
fail s = T $ const (Fail s, IntSet.empty)
{-# INLINE fail #-}

-- | For every path matching the Glob pattern, run the given Task and
--   collect all results in a list.
--   @match@ caches the list, and only triggers the Task on a given path if
--   the underlying file is new or has changed since the last run.
match :: (Binary b, Eq b) => Pattern -> (Task m Path -> Task m b) -> Task m [b]
match pat t = T \n ->
  -- remove locally-bound variables
  -- NOTE(flupe): maybe we can pospone the filtering at evaluation
  --              because maybe it gets in the way of compile-time translation
  let (t', IntSet.filter (< n) -> vst) = unTask (t $ T $ const (Var n, IntSet.empty)) $! n + 1
  in (Match pat t' vst, vst)
{-# INLINE match #-}

-- | Lift a value into a task.
val :: Value a -> Task m a
val !x = T $ const (Val x, IntSet.empty)
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
