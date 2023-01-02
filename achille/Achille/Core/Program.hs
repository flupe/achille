module Achille.Core.Program where

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
  Match  :: (Eq b, Binary b)
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
runProgram = runProgramIn emptyEnv
{-# INLINE runProgram #-}


data BoxedValue = forall a. Boxed { unBox :: {-# UNPACK #-} !(Value a) }

data Env = Env (IntMap BoxedValue) {-# UNPACK #-} !Int

emptyEnv :: Env
emptyEnv = Env IntMap.empty 0

lookupEnv :: Env -> Int -> Maybe a
lookupEnv (Env env n) k = env !? k <&> \(Boxed v) -> unsafeCoerce v

bindEnv :: Env -> Value a -> Env
bindEnv (Env env n) x = Env (IntMap.insert n (Boxed x) env) (n + 1)

envChanged :: Env -> IntSet -> Bool
envChanged (Env env _) = IntSet.foldr' op False
  -- NOTE(flupe): maybe we can early return once we reach True
  where op :: Int -> Bool -> Bool
        op ((env IntMap.!) -> Boxed v) = (|| hasChanged v)

runProgramIn
  :: (Monad m, MonadFail m, AchilleIO m) 
  => Env -> Program m a -> Context -> Cache -> m (Value a, Cache)
runProgramIn env t ctx@Context{..} cache = case t of
  Var k -> case lookupEnv env k of
    Just v   -> pure (v, cache)
    Nothing  -> Prelude.fail $ "Variable " <> show k <> " out of scope. This is a bug, please report!"

  Seq x y -> do
    let (cx, cy) = splitCache cache
    (_ , cx) <- runProgramIn env x ctx cx
    (vy, cy) <- runProgramIn env y ctx cy
    pure (vy, joinCache cx cy)

  Bind x f -> do
    let (cx, cf) = splitCache cache
    (vx , cx) <- runProgramIn env x ctx cx
    (vy , cf) <- runProgramIn (bindEnv env vx) f ctx cf
    pure (vy, joinCache cx cf)

  Match pat (t :: Program m b) vars -> do
    let stored :: Map FilePath (b, Cache) = fromMaybe Map.empty $ fromCache cache
    paths <- sort . (fmap (makeRelative inputRoot)) <$> glob (inputRoot </> currentDir) pat
    res :: [(Value b, Cache)] <- forM paths \src -> do
      mtime <- getModificationTime (inputRoot </> src)
      case stored Map.!? src of
        Just (x, cache') | mtime <= lastTime, not (envChanged env vars) -> pure (value False x, cache')
        mpast -> do
          let cache' = fromMaybe emptyCache $ Prelude.snd <$> mpast
              env'   = bindEnv env (value False src)
          (t, cache'') <- runProgramIn env' t ctx cache'
          -- TODO(flupe): refactor this, make this pretty
          let t' = 
                case mpast of
                  Just r@(ot, ocache) | ot == theVal t -> (value False ot, ocache)
                  _                                    -> (t, cache'')
          pure t'
    let cache' :: Map FilePath (b, Cache) =
          Map.fromList (zip paths $ first theVal <$> res)
    pure (joinValue (Prelude.fst <$> res), toCache cache')

  Match_ pat (t :: Program m b) vars -> do
    let stored :: Map FilePath Cache = fromMaybe Map.empty $ fromCache cache
    paths <- sort . (fmap (makeRelative inputRoot)) <$> glob (inputRoot </> currentDir) pat
    res :: [(FilePath, Cache)] <- forM paths \src -> do
      mtime <- getModificationTime (inputRoot </> src)
      case stored Map.!? src of
        Just cache' | mtime <= lastTime, not (envChanged env vars) -> pure (src, cache')
        mpast ->
          let cache' = fromMaybe emptyCache $ mpast
              env'   = bindEnv env (value False src)
          in (src,) . Prelude.snd <$> runProgramIn env' t ctx cache'
    pure (unit, toCache $ Map.fromList res)

  Apply r x -> do
    let (cx, cr) = splitCache cache
    (vx, cx) <- runProgramIn env x ctx cx
    (vy, cr) <- runRecipe r ctx cr vx
    pure (vy, joinCache cx cr)

  Pair x y -> do
    let (cx, cy) = splitCache cache
    (vx , cx) <- runProgramIn env x ctx cx
    (vy , cy) <- runProgramIn env y ctx cy
    pure (joinValue (vx, vy), joinCache cx cy)

  -- TODO(flupe): error-recovery and propagation
  Fail s -> Prelude.fail s

  Val v -> pure (v, cache)
{-# INLINE runProgramIn #-}
