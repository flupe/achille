{-# LANGUAGE DerivingStrategies #-}
module Achille.Core.Program where

import Prelude hiding ((.), id, seq, (>>=), (>>), fst, snd)

import Control.Category
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class

import GHC.Stack (HasCallStack)
import Data.Binary (Binary)
import Data.Bifunctor (first, bimap)
import Data.Functor ((<&>))
import Data.IntMap.Strict (IntMap, (!?))
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.List (uncons)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (All(..))
import Data.String (fromString)
import Data.Time (UTCTime)

import Unsafe.Coerce (unsafeCoerce)

import Data.IntMap.Strict   qualified as IntMap
import Data.IntSet          qualified as IntSet
import Data.Map.Strict      qualified as Map

import Achille.Context (Context(..))
import Achille.Diffable
import Achille.DynDeps (DynDeps, getFileDeps)
import Achille.IO (AchilleIO)
import Achille.IO qualified as AIO
import Achille.Path
import Achille.Task.Prim
import Achille.Core.Recipe

import Achille.Cache qualified as Cache


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
  For :: Program m [a] -> Program m b
      -> Program m [b]

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

  -- | Executes a program in the current directory of the given path.
  Scoped :: Program m Path -> Program m a -> Program m a

instance Show (Program m a) where
  show p = case p of
    Var k        -> "Var " <> show k
    Seq x y      -> "Seq (" <> show x <> ") (" <> show y <> ")"
    Bind x f     -> "Bind (" <> show x <> ") (" <> show f <> ")"
    For xs f     -> "For " <> show xs <> " (" <> show f <> ")"
    Ite c x y    -> "Ite (" <> show c <> ") (" <> show x <> ") (" <> show y <> ")"
    Cached _ p   -> "Cached _ (" <> show p <> show ")"
    Apply r x    ->  "Apply (" <> show r <> ") (" <> show x <> ")"
    Pair x y     -> "Pair (" <> show x <> ") (" <> show y <> ")"
    Fail s       -> "Fail " <> show s
    Val _        -> "Val"
    Scoped p x   -> "Scoped (" <> show p <> ") (" <> show x <> ")"

-- | Run a program given some context and incoming cache.
runProgram
  :: (Monad m, MonadFail m, AchilleIO m, HasCallStack)
  => Program m a -> PrimTask m (Value a)
runProgram = runProgramIn emptyEnv
{-# INLINE runProgram #-}


data BoxedValue = forall a. Boxed { unBox :: {-# UNPACK #-} !(Value a) }

data Env = Env (IntMap BoxedValue) {-# UNPACK #-} !Int

emptyEnv :: Env
emptyEnv = Env IntMap.empty 0

lookupEnv :: HasCallStack => Env -> Int -> Maybe a
lookupEnv (Env env _) k = env !? k <&> \(Boxed v) -> unsafeCoerce v

bindEnv :: Env -> Value a -> Env
bindEnv (Env env n) x = Env (IntMap.insert n (Boxed x) env) (n + 1)

envChanged :: Env -> IntSet -> Bool
envChanged (Env env _) = IntSet.foldr' op False
  -- NOTE(flupe): maybe we can early return once we reach True
  -- TODO(flupe): we shouldn't ever fail looking up the env,
  --              so we're not filtering enough variables...
  where op :: Int -> Bool -> Bool
        op k b =
          case env IntMap.!? k of
            Just (Boxed v) -> hasChanged v || b
            Nothing        -> b

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
            Just (_, d, c) -> (d     , c               )
            _              -> (mempty, Cache.emptyCache)
    if  isNothing cached
     || envChanged env vs
     || not (depsClean updatedFiles lastTime deps) then do
      -- AIO.log (fromString $ show $ envChanged env vs)
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

  For (xs :: Program m [a]) (f :: Program m b) -> do
    (cxs, cfor)  <- splitCache
    (vxs, cxs') <- withCache cxs $ runProgramIn env xs
    case vxs of
      Nothing -> joinCache cxs' cfor *> halt
      Just (splitValue -> cxs) -> do
        let chunks :: [Cache] = fromMaybe [] $ Cache.fromCache cfor
        (ys, chunks') <- forChanges cxs chunks
        joinCache cxs' (Cache.toCache chunks')
        forward (joinValue <$> ys)
    where
      forChanges :: [ListChange a] -> [Cache] -> PrimTask m (Maybe [ListChange b], [Cache])
      forChanges []           _     = pure (Just [], [])
      forChanges (Deleted   :vs) cs = forChanges vs (drop 1 cs) <&> first (fmap (Deleted:))
      forChanges (Inserted x:vs) cs = do
        let env' = bindEnv env (value False x)
        (y, cy) <- withCache Cache.emptyCache $ runProgramIn env' f
        case y of
          Nothing -> pure (Nothing, cy : cs)
          Just vy -> forChanges vs cs <&> bimap (fmap (Inserted (theVal vy):)) (cy:)
      forChanges (Kept v:vs) cs = do
        let env' = bindEnv env v
        let (cv, cs') = fromMaybe (Cache.emptyCache, []) (uncons cs)
        (y, cy) <- withCache cv $ runProgramIn env' f
        case y of
          Nothing -> pure (Nothing, [cy])
          Just vy -> forChanges vs cs' <&> bimap (fmap (Kept vy:)) (cy:)

  Scoped x y -> do
    (cx, cy) <- splitCache
    (msrc, cx') <- withCache cx $ runProgramIn env x
    case msrc of
      Nothing  -> joinCache cx' cy *> halt
      Just (theVal -> src) -> do
        (b, cy') <- withCache cy $ local (\c -> c {currentDir = takeDirectory src})
                                 $ runProgramIn env y
        joinCache cx' cy'
        forward b
{-# INLINE runProgramIn #-}
