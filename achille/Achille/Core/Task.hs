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
  , for
  , apply
  , val
  , void
  , cached
  , toProgram
  , ifThenElse
  , scoped
  , switch
  , Pattern(Pattern)
  ) where

import Prelude hiding ((.), id, seq, fail, (>>=), (>>), fst, snd)

import Control.Category
import Control.Monad.Reader.Class (reader)
import Control.Applicative (liftA2)
import Control.Arrow
import Data.Binary (Binary)
import Data.IntSet (IntSet)
import Data.String (IsString(fromString))
import GHC.Exts (IsList(..))
import Generics.SOP (NS(..), NP(..), sList, SList(..), I(..), K(..))
import Generics.SOP qualified as SOP

import Achille.Core.Recipe
import Achille.Core.Program
import Achille.Diffable as Diffable
import Achille.DynDeps (DynDeps)
import Achille.Path
import Achille.Task.Prim
import Achille.IO hiding (glob)

import Prelude            qualified
import Data.IntSet        qualified as IntSet
import Achille.Context    qualified as Ctx


-- NOTE(flupe): maybe we should *NOT* make all the applications strict.
--              it's likely that's all that is required to enable recursive tasks.

-- | Core abstraction for build tasks.
newtype Task m a = T { unTask :: Int -> (Program m a, IntSet) }

instance Monad m => Functor (Task m) where
  fmap f = apply (arr f)
  {-# INLINE fmap #-}

instance Monad m => Applicative (Task m) where
  -- NOTE(flupe): values lifted with @pure@ are considered to always be old.
  --              maybe we want to make them always new, but a choice has to be made.
  --              over or under approximating incrementality.
  pure x = val (value False x)
  {-# INLINE pure #-}

  liftA2 f x y = apply (arr (uncurry f)) (pair x y)
  {-# INLINE liftA2 #-}

instance {-# OVERLAPPABLE #-} (Monad m, IsString a) => IsString (Task m a) where
  fromString = pure . fromString
  {-# INLINE fromString #-}

-- NOTE(flupe): Paths and Patterns are not just lifted to tasks, but also get the current
--              directory prepended.
-- NOTE(flupe): is this desired behaviour, actually?

instance {-# OVERLAPPING #-} Monad m => IsString (Task m Path) where
  fromString p = apply rec (pure ())
    where
      rec :: Recipe m () Path
      rec = recipe "Achille.Core.Task.toPath" \_ -> do
        curr <- reader Ctx.currentDir
        let path :: Path = normalise (curr </> fromString p)
        same <- (Just path ==) <$> fromCache
        toCache path
        pure (value (not same) path)

instance Monad m => IsList (Task m [a]) where
  type Item (Task m [a]) = Task m a

  fromList :: [Task m a] -> Task m [a]
  fromList [] = pure []
  fromList (x:xs) = liftA2 (:) x (fromList xs)

  toList :: Task m [a] -> [Task m a]
  toList = error "Cannot pattern match on task."


toProgram :: Task m a -> Program m a
toProgram t = Prelude.fst $! unTask t 0
{-# INLINE toProgram #-}

runTask
  :: (Monad m, MonadFail m, AchilleIO m)
  => Task m a -> Context -> Cache -> m (Maybe (Value a), Cache, DynDeps)
runTask (toProgram -> p) = runPrimTask (runProgram p)
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

for :: Task m [a] -> (Task m a -> Task m b) -> Task m [b]
for (T xs) t = T $ \n ->
  let (xs', vxs) = xs $! n
      (t', IntSet.filter (< n) -> vst) = unTask (t $ T $ const (Var n, IntSet.empty)) $! n + 1
  in (For xs' t', IntSet.union vst vxs)

-- | For every path matching the Glob pattern, run the given Task and
--   collect all results in a list.
--   @match@ caches the list, and only triggers the Task on a given path if
--   the underlying file is new or has changed since the last run.
-- match pat t = T \n ->
--   -- remove locally-bound variables
--   -- NOTE(flupe): maybe we can pospone the filtering at evaluation
--   --              because maybe it gets in the way of compile-time translation
--   let (t', IntSet.filter (< n) -> vst) = unTask (t $ T $ const (Var n, IntSet.empty)) $! n + 1
--   in (Match pat t' vst, vst)
-- {-# INLINE match #-}

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

-- | Conditionally run tasks
ifThenElse :: Monad m => Task m Bool -> Task m a -> Task m a -> Task m a
ifThenElse (T b) (T x) (T y) = T \n ->
  let (b', vsb) = b $! n
      (x', vsx) = x $! n
      (y', vsy) = y $! n
  in (Ite b' x' y', vsb <> vsx <> vsy)
{-# INLINE ifThenElse #-}

cached :: (Monad m, Binary a) => Task m a -> Task m a
cached (T x) = T \n ->
  let (x', vs) = x $! n
  in (Cached vs x', vs)
{-# INLINE cached #-}

scoped :: Monad m => Task m Path -> Task m a -> Task m a
scoped (T x) (T y) = T \n ->
  let (x', vsx) = x $! n
      (y', vsy) = y $! n
  in (Scoped x' y', vsx <> vsy)
{-# INLINE scoped #-}

-- | Encoding of datatype patterns.
newtype Pattern m a = Pattern (NS (NP (Task m)) (SOP.Code a))

-- | Pattern matching on a task producing a SOP-encoded datatype.
switch :: forall a m b. SOP.Generic a => Task m (Lifted a) -> (Pattern m a -> Task m b) -> Task m b
switch (T x) f = T \n ->
  let (x', vsx) = x $! n
      (bs, vsbs) = test2 id n
  in ( Switch x' (Branches bs)
     , vsx <> vsbs
     )
  where
    -- TODO(flupe): write this using SOP combinators
    mkProd :: forall ys. SOP.SListI ys => Int -> (NP (Task m) ys, Int)
    mkProd n = case sList :: SList ys of
      SNil  -> (Nil, n)
      SCons -> 
        let (p, n') = mkProd (n + 1) 
        in (T (const (Var n, IntSet.empty)) :* p, n')

    test :: forall ys. SOP.SListI ys 
         => (NP (Task m) ys -> NS (NP (Task m)) (SOP.Code a))
         -> Int
         -> (Program m b, IntSet)
    test mkSum n = 
      let (prod, n') = mkProd n
      in unTask (f (Pattern $ mkSum prod)) n'

    test2 :: forall xs. SOP.SListI2 xs
          => (NS (NP (Task m)) xs -> NS (NP (Task m)) (SOP.Code a))
          -> Int
          -> (NP (K (Program m b)) xs, IntSet)
    test2 f n = case sList :: SList xs of
      SNil  -> (Nil, IntSet.empty)
      SCons ->
        let (p , vars ) = test (f . Z) n
            (ps, varss) = test2 (f . S) n
        in (K p :* ps, vars <> varss)
{-# INLINE switch #-}

-- convenience mapping  because I don't have time to fully grasp the SOP api
mapNP :: (forall a. f a -> g a) -> NP f xs -> NP g xs
mapNP _ Nil       = Nil
mapNP f (x :* xs) = f x :* mapNP f xs

mapNS ::  (forall a. f a -> g a) -> NS (NP f) xs -> NS (NP g) xs
mapNS f (Z x) = Z (mapNP f x)
mapNS f (S x) = S (mapNS f x)

anyNP :: (forall a. f a -> Bool) -> NP f xs -> Bool
anyNP f Nil = False
anyNP f (x :* xs) = f x || anyNP f xs

anyNS :: (forall a. f a -> Bool) -> NS (NP f) xs -> Bool
anyNS f (Z xs) = anyNP f xs
anyNS f (S xs) = anyNS f xs


