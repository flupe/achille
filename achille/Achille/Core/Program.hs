{-# LANGUAGE DerivingStrategies #-}
module Achille.Core.Program where

import Prelude hiding ((.), id, seq, (>>=), (>>), fst, snd)

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
import Data.Maybe (fromMaybe, catMaybes)
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

import Achille.Config (Config(..))
import Achille.Context (Context(..))
import Achille.Diffable
import Achille.DynDeps (DynDeps, getFileDeps, dependsOnPattern)
import Achille.IO (AchilleIO)
import Achille.Path
import Achille.Task.Prim
import Achille.Core.Recipe

import Achille.Cache qualified as Cache
import Achille.IO    qualified as AIO


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
  => Program m a -> PrimTask m (Value a)
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
  => Env -> Program m a -> PrimTask m (Value a)
runProgramIn env t = case t of

  Var k -> case lookupEnv env k of
    Just v  -> pure v
    Nothing -> fail $ "Variable " <> show k <> " out of scope. This is a bug, please report!"

  Seq x y -> do
    (cx, cy) <- splitCache
    (_,  cx) <- withCache cx $ runProgramIn env x
    (vy, cy) <- withCache cy $ runProgramIn env y
    joinCache cx cy
    forward vy

  Bind x f -> do
    (cx, cf) <- splitCache
    (vx, cx) <- withCache cx $ runProgramIn env x
    case vx of
      Nothing -> joinCache cx cf *> halt
      Just vx -> do
        (vy, cf) <- withCache cf $ runProgramIn (bindEnv env vx) f
        joinCache cx cf
        forward vy
    -- TODO(flupe): propagate failure to environment
    --              to allow things that do not depend on the value to be evaluated

  Fail s -> fail s
  --
  -- TODO(flupe): does this have to be a primitive of Program? what about lists? maps? 
  --              this is almost identical to Seq
  -- TODO(flupe): parallelism
  Pair x y -> do
    (cx, cy) <- splitCache
    (a, cx) <- withCache cx $ runProgramIn env x
    (b, cy) <- withCache cy $ runProgramIn env y
    joinCache cx cy
    forward (joinValue <$> ((,) <$> a <*> b))

  Val v -> pure v

  Apply r x -> do
    (cx, cr) <- splitCache
    (a, cx) <- withCache cx $ runProgramIn env x
    case a of
      Nothing -> joinCache cx cr *> halt
      Just a -> do
        (b, cr) <- withCache cr $ runRecipe r a
        joinCache cx cr
        forward b

  Match pat (t :: Program m b) vars -> do
    context@Context{..} :: Context <- ask
    let Config{..} = siteConfig
    let thepat = FP.normalise (toFilePath currentDir <> "/" <> Glob.decompile pat)
    let pat' = Glob.simplify $ Glob.compile thepat -- NOTE(flupe): Glob.simplify doesn't do anything?
    stored :: Map Path Cache <- fromMaybe Map.empty <$> fromCache
    paths <- sort . fmap (normalise . makeRelative contentDir) <$> AIO.glob contentDir pat'
    res :: [(Maybe (Value b), Cache)] <- forM paths \src -> do
      mtime <- AIO.getModificationTime (contentDir </> src)
      let currentDir = takeDirectory src
      let fileCache = stored Map.!? src
      case liftA2 (,) fileCache (Cache.fromCache =<< fileCache) :: Maybe (Cache, (b, DynDeps, Cache)) of
        Just (cache, (x, deps, _))
          | mtime <= lastTime
          , not (envChanged env vars)
          , depsClean updatedFiles lastTime deps ->
              tell deps
              $> (Just (value False x), cache)
        mpast -> do
          let (oldVal, cache) = case mpast of
                Just (_, (x, _, cache)) -> (Just x, cache)
                Nothing                 -> (Nothing, Cache.emptyCache)
              env' = bindEnv env (value False src)
          --(t, cache') <-
          local (\c -> c { currentDir = currentDir
                         , updatedFiles = Map.insert (contentDir </> src) mtime updatedFiles
                         })
            $ withCache cache $ runProgramIn env' t
          -- pure ( value (Just (theVal t) == oldVal) (theVal t)
          --      , toCache (theVal t, deps, cache')
          --      )
    let (values, caches) = unzip res
    tell (dependsOnPattern pat')
    toCache (Map.fromAscList (zip paths caches))
    pure (joinValue (catMaybes values))

{-# INLINE runProgramIn #-}
