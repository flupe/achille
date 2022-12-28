{-# LANGUAGE GADTs, Rank2Types, BangPatterns, RecordWildCards #-}

module Achille.Task
  ( Task
  , toTask
  , -- * Running tasks
    --
    -- $running
    runTask
  ) where

import Prelude hiding ((.))
import Control.Category
import Control.Monad (forM)
import Control.Applicative (Alternative, empty)
import Data.Bifunctor (first)
import Data.Binary (Binary)
import Data.IntMap.Strict (IntMap, (!?))
import Data.IntSet (IntSet)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import System.FilePath.Glob (Pattern)
import Unsafe.Coerce (unsafeCoerce)

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet        qualified as IntSet
import Data.Map.Strict    qualified as Map

import Achille.Cache
import Achille.Diffable
import Achille.IO
import Achille.Recipe (Context(..), Recipe(Id), runRecipe)
import Achille.Syntax (Program, Achille)
import Achille.Syntax qualified as Syntax


-- | Tasks are the atomic build operations of achille.
data Task m a where
  Var :: {-# UNPACK #-} !Int -- ^ de Bruijn level
      -> Task m a

  -- Sequencing
  Seq  :: Task m a -> Task m b -> Task m b
  Bind :: Task m a
       -> Task m b -- ^ has a value of type @a@ in scope
       -> Task m b

  -- File matching
  Match  :: Binary b
         => !Pattern
         -> Task m b -- ^ has a filepath in scope
         -> IntSet
         -> Task m [b]

  Match_ :: !Pattern
         -> Task m b -- ^ has a filepath in scope
         -> IntSet
         -> Task m ()

  -- Embedding recipes
  Apply :: !(Recipe m a b) -> Task m a -> Task m b

  -- NOTE(flupe): maybe the following could be moved out of the class?

  Val  :: !(Value a) -> Task m a
  Pair :: Task m a -> Task m b -> Task m (a, b)
  Fail :: !String -> Task m a

instance Show (Task m a) where
  show (Var k) = "Var " <> show k
  show (Seq x y) = "Seq (" <> show x <> ") (" <> show y <> ")"
  show (Bind x f) = "Bind (" <> show x <> ") (" <> show f <> ")"
  show (Match p v t) = "Match " <> show p <> " (" <> show v <> ") (" <> show t <> ")"
  show (Match_ p v t) = "Match_ " <> show p <> " (" <> show v <> ") (" <> show t <> ")"
  show (Apply r x) = "Apply (" <> show r <> ") (" <> show x <> ")"
  show (Pair x y) = "Pair (" <> show x <> ") (" <> show y <> ")"
  show (Fail s) = "Fail " <> show s
  show (Val x) = "Val"

-- | Convert a user program to its internal @Task@ representation.
toTask :: Program m a -> Task m a
toTask p = fst $! unDB p 0
{-# INLINE toTask #-}


newtype DB m a = DB { unDB :: Int -> (Task m a, IntSet) }

instance Achille DB where
  DB x >> DB y = DB \n ->
    let (x', vsx) = x $! n
        (y', vsy) = y $! n
    in (seq x' y', vsx <> vsy)
    where seq :: Task m a -> Task m b -> Task m b
          seq (Seq x y) z = seq x (seq y z)
          {-# INLINE seq #-}
  {-# INLINE (>>) #-}
  DB x >>= f = DB \n -> 
    let (x', vsx) = x $! n
        (f', vsf) = unDB (f $ DB \_ -> (Var n, IntSet.singleton n)) $! n + 1
    in (Bind x' f', vsx <> vsf)
  {-# INLINE (>>=) #-}
  pair (DB x) (DB y) = DB \n -> 
    let (x', vsx) = x $! n
        (y', vsy) = y $! n
    in (Pair x' y', vsx <> vsy)
  {-# INLINE pair #-}
  fail s = DB \_ -> (Fail s, IntSet.empty)
  {-# INLINE fail #-}
  match pat t = DB \n ->
    let (t', vst) = unDB (t $ DB \_ -> (Var n, IntSet.empty)) $! n + 1
    in (Match pat t' vst, vst)
  {-# INLINE match #-}
  match_ pat t = DB \n -> 
    let (t', vst) = unDB (t $ DB \_ -> (Var n, IntSet.empty)) $! n + 1
    in (Match_ pat t' vst, vst)
  {-# INLINE match_ #-}
  apply !r (DB x) = DB \n -> 
    let (x', vsx) = x $! n in (app r x', vsx)
    where app :: Recipe m a b -> Task m a -> Task m b
          app r (Apply s x) = app (r . s) x
          app Id x = x
          app r x = Apply r x
          {-# INLINE app #-}
  {-# INLINE apply #-}
  val !x = DB \_ -> (Val x, IntSet.empty)
  {-# INLINE val #-}


infixr 3 ?*>
(?*>) :: Alternative f => Bool -> f a -> f a
b ?*> x = if b then x else empty
{-# INLINE (?*>) #-}

sameOld :: a -> Value a
sameOld x = value x False
{-# INLINE sameOld #-}


-- $running
--
-- Once a user program has been converted into a task (hopefully at compile-time),
-- is is possible to execute it.

-- | Run a task given some context and incoming cache.
runTask
  :: (Monad m, MonadFail m, AchilleIO m)
  => Context -> Cache -> Task m a -> m (Value a, Cache)
runTask = runTaskIn IntMap.empty 0
{-# INLINE runTask #-}


data BoxedValue = forall a. Boxed { unBox :: {-# UNPACK #-} !(Value a) }

type Env = IntMap BoxedValue

runTaskIn
  :: (Monad m, MonadFail m, AchilleIO m) 
  => Env -> Int -> Context -> Cache -> Task m a -> m (Value a, Cache)
runTaskIn env !depth ctx@Context{..} cache t = case t of
  Var k -> case env !? k of
    Just (Boxed v) -> pure (unsafeCoerce v, cache)
    Nothing        -> fail $ "Variable " <> show k <> " out of scope. This is a bug, please report!"

  -- make Seqs right-nested to enforce associativity of cache
  Seq (Seq x y) z -> runTaskIn env depth ctx cache (Seq x (Seq y z))
  Seq x y -> do
    let (cx, cy) = splitCache cache
    (_ , cx) <- runTaskIn env depth ctx cx x
    (vy, cy) <- runTaskIn env depth ctx cy y
    pure (vy, joinCache cx cy)

  Bind x f -> do
    let (cx, cf) = splitCache cache
    (vx , cx) <- runTaskIn env depth ctx cx x
    (vy , cf) <- runTaskIn (IntMap.insert depth (Boxed vx) env) (depth + 1) ctx cf f
    pure (vy, joinCache cx cf)

  Match pat (t :: Task m b) vars -> do
    -- TODO (flupe): watch variable use in t
    let stored :: Map FilePath (b, Cache) = fromMaybe Map.empty $ fromCache cache
    paths <- sort . (fmap (currentDir </>)) <$> glob (inputRoot </> currentDir) pat
    res :: [(Value b, Cache)] <- forM paths \src -> do
      mtime <- getModificationTime (inputRoot </> src)
      case stored Map.!? src of
        Just (x, cache') | mtime <= lastTime -> pure (sameOld x, cache')
        mpast ->
          let cache' = fromMaybe emptyCache $ snd <$> mpast
              env'   = IntMap.insert depth (Boxed $ sameOld src) env
          in runTaskIn env' (depth + 1) ctx cache' t
    let cache' :: Map FilePath (b, Cache) =
          Map.fromList (zip paths $ first fst <$> res)
    pure (joinList (fst <$> res), toCache cache')

  Match_ pat (t :: Task m b) vars -> do
    -- TODO (flupe): watch variable use in t
    let stored :: Map FilePath Cache = fromMaybe Map.empty $ fromCache cache
    paths <- sort . (fmap (currentDir </>)) <$> glob (inputRoot </> currentDir) pat
    res :: [(FilePath, Cache)] <- forM paths \src -> do
      mtime <- getModificationTime (inputRoot </> src)
      case stored Map.!? src of
        Just cache' | mtime <= lastTime -> pure (src, cache')
        mpast ->
          let cache' = fromMaybe emptyCache $ mpast
              env'   = IntMap.insert depth (Boxed $ sameOld src) env
          in (src,) . snd <$> runTaskIn env' (depth + 1) ctx cache' t
    pure (unit, toCache $ Map.fromList res)

  Apply r x -> do
    let (cx, cr) = splitCache cache
    (vx, cx) <- runTaskIn env depth ctx cx x
    (vy, cr) <- runRecipe ctx cr vx r
    pure (vy, joinCache cx cr)

  Pair x y -> do
    let (cx, cy) = splitCache cache
    (vx , cx) <- runTaskIn env depth ctx cx x
    (vy , cy) <- runTaskIn env depth ctx cy y
    pure (joinPair vx vy, joinCache cx cy)

  -- TODO(flupe): error-recovery and propagation
  Fail s -> Prelude.fail s

  Val v -> pure (v, cache)
{-# INLINE runTaskIn #-}

