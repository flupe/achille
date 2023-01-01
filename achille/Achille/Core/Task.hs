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
  ) where

import Prelude hiding ((.), id, seq, fail, (>>=), (>>), fst, snd)

import Control.Category
import Control.Monad (forM)
import Control.Applicative (Alternative, empty, liftA2)
import Control.Arrow

import Data.Binary (Binary)
import Data.IntMap.Strict (IntMap, (!?))
import Data.IntSet (IntSet)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Data.Time (UTCTime)

import System.FilePath ((</>))
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


-- | @Program m a@ is the internal representaion of tasks in achille.
data Program m a where
  Var :: {-# UNPACK #-} !Int -- ^ de Bruijn level
      -> Program m a

  -- Sequencing
  Seq  :: Program m a -> Program m b -> Program m b
  Bind :: Program m a
       -> Program m b -- ^ has a value of type @a@ in scope
       -> Program m b

  -- File matching
  Match  :: Binary b
         => !Pattern
         -> Program m b -- ^ has a filepath in scope
         -> IntSet
         -> Program m [b]

  Match_ :: !Pattern
         -> Program m b -- ^ has a filepath in scope
         -> IntSet
         -> Program m ()

  -- Embedding recipes
  Apply :: !(Recipe m a b) -> Program m a -> Program m b

  -- NOTE(flupe): maybe the following could be moved out of the class?

  Val  :: !(Value a) -> Program m a
  Pair :: Program m a -> Program m b -> Program m (a, b)
  Fail :: !String -> Program m a


instance Show (Program m a) where
  show p = case p of
    Var k        -> "Var " <> show k
    Seq x y      -> "Seq (" <> show x <> ") (" <> show y <> ")"
    Bind x f     -> "Bind (" <> show x <> ") (" <> show f <> ")"
    Match p v t  -> "Match " <> show p <> " (" <> show v <> ") (" <> show t <> ")"
    Match_ p v t -> "Match_ " <> show p <> " (" <> show v <> ") (" <> show t <> ")"
    Apply r x    ->  "Apply (" <> show r <> ") (" <> show x <> ")"
    Pair x y     -> "Pair (" <> show x <> ") (" <> show y <> ")"
    Fail s       -> "Fail " <> show s
    Val x        -> "Val"


-- | Run a program given some context and incoming cache.
runProgram
  :: (Monad m, MonadFail m, AchilleIO m)
  => Program m a -> Context -> Cache -> m (Value a, Cache)
runProgram = runProgramIn IntMap.empty 0
{-# INLINE runProgram #-}

data BoxedValue = forall a. Boxed { unBox :: {-# UNPACK #-} !(Value a) }
type Env = IntMap BoxedValue

envChanged :: Env -> IntSet -> Bool
envChanged env = IntSet.foldr' op False
  -- NOTE(flupe): maybe we can early return once we reach True
  where op :: Int -> Bool -> Bool
        op ((env IntMap.!) -> Boxed v) = (|| hasChanged v)

runProgramIn
  :: (Monad m, MonadFail m, AchilleIO m) 
  => Env -> Int -> Program m a -> Context -> Cache -> m (Value a, Cache)
runProgramIn env !depth t ctx@Context{..} cache = case t of
  Var k -> case env !? k of
    Just (Boxed v) -> pure (unsafeCoerce v, cache)
    Nothing        -> Prelude.fail $ "Variable " <> show k <> " out of scope. This is a bug, please report!"

  Seq x y -> do
    let (cx, cy) = splitCache cache
    (_ , cx) <- runProgramIn env depth x ctx cx
    (vy, cy) <- runProgramIn env depth y ctx cy
    pure (vy, joinCache cx cy)

  Bind x f -> do
    let (cx, cf) = splitCache cache
    (vx , cx) <- runProgramIn env depth x ctx cx
    (vy , cf) <- runProgramIn (IntMap.insert depth (Boxed vx) env) (depth + 1) f ctx cf
    pure (vy, joinCache cx cf)

  Match pat (t :: Program m b) vars -> do
    let stored :: Map FilePath (b, Cache) = fromMaybe Map.empty $ fromCache cache
    paths <- sort . (fmap (currentDir </>)) <$> glob (inputRoot </> currentDir) pat
    res :: [(Value b, Cache)] <- forM paths \src -> do
      mtime <- getModificationTime (inputRoot </> src)
      case stored Map.!? src of
        Just (x, cache') | mtime <= lastTime, not (envChanged env vars) -> pure (value False x, cache')
        mpast ->
          let cache' = fromMaybe emptyCache $ Prelude.snd <$> mpast
              env'   = IntMap.insert depth (Boxed $ value False src) env
          in runProgramIn env' (depth + 1) t ctx cache'
    let cache' :: Map FilePath (b, Cache) =
          Map.fromList (zip paths $ first theVal <$> res)
    pure (joinValue (Prelude.fst <$> res), toCache cache')

  Match_ pat (t :: Program m b) vars -> do
    let stored :: Map FilePath Cache = fromMaybe Map.empty $ fromCache cache
    paths <- sort . (fmap (currentDir </>)) <$> glob (inputRoot </> currentDir) pat
    res :: [(FilePath, Cache)] <- forM paths \src -> do
      mtime <- getModificationTime (inputRoot </> src)
      case stored Map.!? src of
        Just cache' | mtime <= lastTime, not (envChanged env vars) -> pure (src, cache')
        mpast ->
          let cache' = fromMaybe emptyCache $ mpast
              env'   = IntMap.insert depth (Boxed $ value False src) env
          in (src,) . Prelude.snd <$> runProgramIn env' (depth + 1) t ctx cache'
    pure (unit, toCache $ Map.fromList res)

  Apply r x -> do
    let (cx, cr) = splitCache cache
    (vx, cx) <- runProgramIn env depth x ctx cx
    (vy, cr) <- runRecipe r ctx cr vx
    pure (vy, joinCache cx cr)

  Pair x y -> do
    let (cx, cy) = splitCache cache
    (vx , cx) <- runProgramIn env depth x ctx cx
    (vy , cy) <- runProgramIn env depth y ctx cy
    pure (joinValue (vx, vy), joinCache cx cy)

  -- TODO(flupe): error-recovery and propagation
  Fail s -> Prelude.fail s

  Val v -> pure (v, cache)
{-# INLINE runProgramIn #-}

-- NOTE(flupe): maybe we should *NOT* make all the applications strict.
--              it's likely that's all that is required to enable recursive tasks.

-- | Core abstraction for build tasks.
newtype Task m a = T { unTask :: Int -> (Program m a, IntSet) }

runTask
  :: (Monad m, MonadFail m, AchilleIO m)
  => Task m a -> Context -> Cache -> m (Value a, Cache)
runTask t = runProgram (Prelude.fst $! unTask t 0)
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
match :: Binary b => Pattern -> (Task m FilePath -> Task m b) -> Task m [b]
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
