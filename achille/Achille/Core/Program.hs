{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
module Achille.Core.Program where

import Prelude hiding ((.), id, seq, (>>=), (>>), fst, snd)

import Control.Applicative (liftA2)
import Control.Category
import Control.Monad (forM)
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class

import Data.Binary (Binary)
import Data.Functor ((<&>), ($>))
import Data.IntMap.Strict (IntMap, (!?))
import Data.IntSet (IntSet)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Data.Monoid (All(..))
import Data.Time (UTCTime)

import System.FilePath.Glob (Pattern)
import Unsafe.Coerce (unsafeCoerce)

import Data.IntMap.Strict   qualified as IntMap
import Data.IntSet          qualified as IntSet
import Data.Set             qualified as Set
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

  -- Iteration
  For :: (Binary a, Ord a, Eq b, Binary b)
      => Program m [a] -> Program m b -> IntSet
      -> Program m [b]

  -- -- File matching
  -- Match  :: (Eq b, Binary b)
  --        => !Pattern
  --        -> Program m b -- ^ has a filepath in scope
  --        -> IntSet
  --        -> Program m [b]

  -- Conditional branching
  Ite :: Program m Bool -> Program m a -> Program m a -> Program m a

  -- Caching combinator
  Cached :: Binary a => IntSet -> Program m a -> Program m a

  -- Embedding recipes
  Apply :: !(Recipe m a b) -> Program m a -> Program m b

  -- NOTE(flupe): maybe the following could be moved out of the class?

  Val  :: !(Value a) -> Program m a
  Pair :: Program m a -> Program m b -> Program m (a, b)
  Fail :: !String -> Program m a


instance Show (Program m a) where
  show p = case p of
    Var k         -> "Var " <> show k
    Seq x y       -> "Seq (" <> show x <> ") (" <> show y <> ")"
    Bind x f      -> "Bind (" <> show x <> ") (" <> show f <> ")"
    For xs f vs  -> "Match " <> show xs <> " (" <> show f <> ") (" <> show vs <> ")"
    Ite c x y     -> "Ite (" <> show c <> ") (" <> show x <> ") (" <> show y <> ")"
    Cached _ p    -> "Cached _ (" <> show p <> show ")"
    Apply r x     ->  "Apply (" <> show r <> ") (" <> show x <> ")"
    Pair x y      -> "Pair (" <> show x <> ") (" <> show y <> ")"
    Fail s        -> "Fail " <> show s
    Val _         -> "Val"

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
lookupEnv (Env env _) k = env !? k <&> \(Boxed v) -> unsafeCoerce v

bindEnv :: Env -> Value a -> Env
bindEnv (Env env n) x = Env (IntMap.insert n (Boxed x) env) (n + 1)

envChanged :: Env -> IntSet -> Bool
envChanged (Env env _) = IntSet.foldr' op False
  -- NOTE(flupe): maybe we can early return once we reach True
  where op :: Int -> Bool -> Bool
        op ((env IntMap.!) -> Boxed v) = (|| hasChanged v)

depsClean :: Map Path UTCTime -> UTCTime -> DynDeps -> Bool
depsClean edits lastT (getFileDeps -> fdeps) = getAll $ foldMap (All . isClean) fdeps
  where isClean :: Path -> Bool
        isClean src = maybe False (<= lastT) (edits Map.!? src)

runProgramIn
  :: (Monad m, MonadFail m, AchilleIO m)
  => Env -> Program m a -> PrimTask m (Value a)
runProgramIn env t = case t of

  Var k -> maybe halt pure $ lookupEnv env k

  Seq x y -> do
    (cx, cy) <- splitCache
    (_,  cx') <- withCache cx $ runProgramIn env x
    (vy, cy') <- withCache cy $ runProgramIn env y
    joinCache cx' cy'
    forward vy

  Bind x f -> do
    (cx, cf) <- splitCache
    (vx, cx') <- withCache cx $ runProgramIn env x
    let env' = maybe env (bindEnv env) vx
    (vy, cf') <- withCache cf $ runProgramIn env' f
    joinCache cx' cf'
    forward vy
    -- TODO(flupe): propagate failure to environment
    --              to allow things that do not depend on the value to be evaluated

  Cached vs (p :: Program m a) -> do
    Context{updatedFiles, lastTime} :: Context <- ask
    cached :: Maybe (a, DynDeps, Cache) <- fromCache
    let (deps, cache) :: (DynDeps, Cache) =
          case cached of
            Just (_, d, c) -> (d, c)
            _              -> (mempty, Cache.emptyCache)
    if isNothing cached
        || envChanged env vs
        || not (depsClean updatedFiles lastTime deps) then do
      ((v, cache'), deps) <- listen $ withCache cache $ runProgramIn env p
      case v of
        Nothing -> forward v
        Just x  -> toCache (theVal x, deps, cache') *> forward v
    else do
      let Just (x, deps, _) = cached
      tell deps
      forward (Just (value False x))

  Ite c x y -> do
    (cc, cxy) <- splitCache
    let (cx, cy) = Cache.splitCache cxy
    (mc, cc') <- withCache cc $ runProgramIn env c
    case mc of
      Nothing -> joinCache cc' cxy *> halt
      Just vb ->
        if theVal vb then do
          (vx, cx') <- withCache cx $ runProgramIn env x
          joinCache cc' (Cache.joinCache cx' cy)
          forward vx
        else do
          (vy, cy') <- withCache cy $ runProgramIn env y
          joinCache cc' (Cache.joinCache cx cy')
          forward vy

  Fail s -> fail s
  --
  -- TODO(flupe): does this have to be a primitive of Program? what about lists? maps? 
  --              this is almost identical to Seq
  -- TODO(flupe): parallelism
  Pair x y -> do
    (cx, cy) <- splitCache
    (a, cx') <- withCache cx $ runProgramIn env x
    (b, cy') <- withCache cy $ runProgramIn env y
    joinCache cx' cy'
    forward (joinValue <$> ((,) <$> a <*> b))

  Val v -> pure v

  Apply r x -> do
    (cx, cr) <- splitCache
    (ma, cx') <- withCache cx $ runProgramIn env x
    case ma of
      Nothing -> joinCache cx' cr *> halt
      Just a -> do
        (b, cr') <- withCache cr $ runRecipe r a
        joinCache cx' cr'
        forward b

  For (xs :: Program m [a]) (f :: Program m b) vs -> do
    (cxs, cys)  <- splitCache
    (vxs, cxs') <- withCache cxs $ runProgramIn env xs
    case vxs of
      -- failure of getting input
      Nothing -> joinCache cxs' cys *> halt
      Just (splitValue -> cxs) -> do
        chunks :: Map a (b, DynDeps, Cache) <- fromMaybe Map.empty <$> fromCache
        rezz :: [(ListChange b, Maybe (a, (b, DynDeps, Cache)))] <- forM cxs \case
          Deleted k -> let (x, _, _) = chunks Map.! k in pure (Deleted x, Nothing) -- in the case were deleted info is given, it should already be in the cache
          Inserted k -> do
            ((v, cache), deps) <- listen $ withCache Cache.emptyCache $ runProgramIn env f
            tell deps
            case v of
              Nothing -> halt
              Just v  -> pure (Inserted (theVal v), Just (k, (theVal v, deps, cache)))
          Kept vk ->
            case chunks Map.!? theVal vk of
              Just (cachedVal, deps, cache) -> undefined
              Nothing -> undefined
        let (changes, catMaybes -> items) = unzip rezz
        toCache (Map.fromAscList items) -- unnecessary conversion??
        pure (joinValue changes)

  -- Match pat (t :: Program m b) vars -> do
  --   context@Context{..} :: Context <- ask
  --   undefined
    -- let Config{..} = siteConfig
    -- let thepat = toFilePath currentDir FP.</> Glob.decompile pat
    -- let pat' = Glob.simplify $ Glob.compile thepat -- NOTE(flupe): Glob.simplify doesn't do anything?
    -- stored :: Map Path Cache <- fromMaybe Map.empty <$> fromCache

    -- -- TODO: make this cleaner
    -- current_paths <- Set.fromList . sort . fmap (normalise . makeRelative contentDir)
    --           <$> AIO.glob contentDir pat'
    -- let old_paths = Map.keysSet stored
    --     all_paths = Set.union current_paths old_paths

    -- -- invariant: A cache is returned iff the listchange is Inserted or Kept
    -- res :: [(Maybe (ListChange b), Maybe Cache)]
    --   <- Set.toList <$> forM all_paths \src -> do
    --     mtime <- AIO.getModificationTime (contentDir </> src)
    --     let currentDir = takeDirectory src
    --     let fileCache = stored Map.!? src
    --     case liftA2 (,) fileCache (Cache.fromCache =<< fileCache) :: Maybe (Cache, (b, DynDeps, Cache)) of
    --       Just (cache, (x, deps, _))
    --         | mtime <= lastTime
    --         , not (envChanged env vars)
    --         , depsClean updatedFiles lastTime deps ->
    --             tell deps $> undefined -- (Just (value False x), cache)
    --       mpast -> do
    --         let (oldVal, cache) = case mpast of
    --               Just (_, (x, _, cache)) -> (Just x, cache)
    --               Nothing                 -> (Nothing, Cache.emptyCache)
    --             env' = bindEnv env (value False src)
    --         ((t, cache'), deps) <- listen $
    --           local (\c -> c { currentDir = currentDir
    --                          , updatedFiles = Map.insert (contentDir </> src) mtime updatedFiles
    --                          })
    --             $ withCache cache $ runProgramIn env' t
    --         case t of
    --           Nothing -> undefined -- pure (Nothing, Cache.emptyCache)
    --           Just t  -> undefined -- pure (Just t, Cache.toCache (theVal t, deps, cache'))
    --         -- pure ( value (Just (theVal t) == oldVal) (theVal t)
    --         --      , toCache (theVal t, deps, cache')
    --         --      )
    -- tell (dependsOnPattern pat')
    -- let (values, caches) = unzip res
    -- toCache (Map.fromAscList (zip paths caches))
    -- pure (joinValue (catMaybes values))

{-# INLINE runProgramIn #-}
