{-# LANGUAGE DerivingStrategies #-}
module Achille.Core.Program where

import Prelude hiding ((.), id, seq, fail, (>>=), (>>), fst, snd)

import Control.Applicative (Alternative, empty, liftA2)
import Control.Arrow
import Control.Category
import Control.Monad (forM)
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class

import Data.Binary (Binary)
import Data.Foldable (fold)
import Data.Functor ((<&>), ($>))
import Data.IntMap.Strict (IntMap, (!?))
import Data.IntSet (IntSet)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (All(..))
import Data.String (IsString(fromString))
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import System.FilePath.Glob (Pattern)
import Unsafe.Coerce (unsafeCoerce)

import Prelude              qualified
import Data.IntMap.Strict   qualified as IntMap
import Data.IntSet          qualified as IntSet
import Data.Map.Strict      qualified as Map
import System.FilePath      qualified as FP
import System.FilePath.Glob qualified as Glob

import Achille.Cache
import Achille.Config
import Achille.Context (Context(..))
import Achille.Diffable
import Achille.DynDeps (DynDeps, getFileDeps, dependsOnPattern)
import Achille.IO (AchilleIO)
import Achille.Path
import Achille.Result
import Achille.Core.Recipe

import Achille.IO qualified as AIO


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
    Apply r x    ->  "Apply (" <> show r <> ") (" <> show x <> ")"
    Pair x y     -> "Pair (" <> show x <> ") (" <> show y <> ")"
    Fail s       -> "Fail " <> show s
    Val x        -> "Val"


-- | Run a program given some context and incoming cache.
runProgram
  :: (Monad m, MonadFail m, AchilleIO m)
  => Program m a -> Cache -> Result m (Value a, Cache)
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

depsClean :: Map Path UTCTime -> UTCTime -> DynDeps -> Bool
depsClean edits lastTime (getFileDeps -> fdeps) = getAll $ foldMap (All . isClean) fdeps
  where isClean :: Path -> Bool
        isClean src = maybe False (<= lastTime) (edits Map.!? src)

runProgramIn
  :: (Monad m, MonadFail m, AchilleIO m)
  => Env -> Program m a -> Cache -> Result m (Value a, Cache)
runProgramIn env t cache = case t of

  Var k -> case lookupEnv env k of
    Just v  -> pure (v, cache)
    Nothing -> Prelude.fail $ "Variable " <> show k <> " out of scope. This is a bug, please report!"

  Seq x y -> do
    let (cx, cy) = splitCache cache
    (_,  cx) <- runProgramIn env x cx
    (vy, cy) <- runProgramIn env y cy
    pure (vy, joinCache cx cy)

  Bind x f -> do
    let (cx, cf) = splitCache cache
    (vx, cx) <- runProgramIn env x cx
    (vy, cf) <- runProgramIn (bindEnv env vx) f cf
    pure (vy, joinCache cx cf)

  -- TODO(flupe): error-recovery and propagation
  Fail s -> Prelude.fail s
  --
  -- TODO(flupe): does this have to be a primitive of Program? what about lists? maps? 
  --              this is almost identical to Seq
  -- TODO(flupe): parallelism
  Pair x y -> do
    let (cx, cy) = splitCache cache
    (a, cx) <- runProgramIn env x cx
    (b, cy) <- runProgramIn env y cy
    pure (joinValue (a, b), joinCache cx cy)

  Val v -> pure (v, cache)

  Apply r x -> do
    let (cx, cr) = splitCache cache
    (a, cx) <- runProgramIn env x cx
    (b, cr) <- runRecipe r cr a
    pure (b, joinCache cx cr)

  Match pat (t :: Program m b) vars -> do
    context@Context{..} :: Context <- ask
    let Config{..} = siteConfig
    let thepat = FP.normalise (toFilePath currentDir <> "/" <> Glob.decompile pat)
    let pat' = Glob.simplify $ Glob.compile thepat -- NOTE(flupe): Glob.simplify doesn't do anything?
    let stored :: Map Path Cache = fromMaybe Map.empty (fromCache cache)
    paths <- sort . fmap (normalise . makeRelative contentDir) <$> AIO.glob contentDir pat'
    res :: [(Value b, Cache)] <- forM paths \src -> do
      mtime <- AIO.getModificationTime (contentDir </> src)
      let currentDir = takeDirectory src
      let fileCache = stored Map.!? src
      case liftA2 (,) fileCache (fromCache =<< fileCache) :: Maybe (Cache, (b, DynDeps, Cache)) of
        Just (cache, (x, deps, _))
          | mtime <= lastTime
          , not (envChanged env vars)
          , depsClean updatedFiles lastTime deps -> tell deps $> (value False x, cache)
        mpast -> do
          let (oldVal, cache) = case mpast of
                Just (_, (x, _, cache)) -> (Just x, cache)
                Nothing                 -> (Nothing, emptyCache)
              env' = bindEnv env (value False src)
          ((t, cache'), deps) <- listen $
            local (\c -> c { currentDir = currentDir
                           , updatedFiles = Map.insert src mtime updatedFiles
                           })
            $ runProgramIn env' t cache
          pure ( value (Just (theVal t) == oldVal) (theVal t)
               , toCache (theVal t, deps, cache')
               )
    let (values, caches) = unzip res
    tell (dependsOnPattern pat')
    pure (joinValue values, toCache (Map.fromAscList (zip paths caches)))

{-# INLINE runProgramIn #-}
