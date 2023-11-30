{-# LANGUAGE DerivingStrategies #-}
module Achille.Core.Program where

import Prelude hiding ((.), id, seq, (>>), fst, snd)
import Prelude qualified as Prelude

import Control.Category
import Control.Monad (foldM)
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Concurrent (MVar)

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
import Data.Time.Clock (UTCTime(UTCTime))

import Unsafe.Coerce (unsafeCoerce)

import Data.IntMap.Strict   qualified as IntMap
import Data.IntSet          qualified as IntSet
import Data.Binary.Instances ()
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


data BoxedValue =
  forall a. Boxed { unBox      :: {-# UNPACK #-} !(Value a)
                  , lastChange :: UTCTime
                  }

data Env = Env (IntMap (MVar (Maybe BoxedValue))) {-# UNPACK #-} !Int

emptyEnv :: Env
emptyEnv = Env IntMap.empty 0

lookupEnv :: (Monad m, AchilleIO m) => Env -> Int -> m (Maybe a)
lookupEnv (Env env _) k
  | Just var <- env !? k = AIO.readMVar var >>= \case
      Just (Boxed v _) -> pure (Just (unsafeCoerce v))
      Nothing          -> pure Nothing
lookupEnv _ _ = pure Nothing

bindEnv :: (Monad m, AchilleIO m) => Env -> m (MVar (Maybe BoxedValue), Env)
bindEnv (Env env n) = do
  var <- AIO.newEmptyMVar
  pure (var, Env (IntMap.insert n var env) (n + 1))

envChanged :: (Monad m, AchilleIO m) => Env -> UTCTime -> IntSet -> m Bool
envChanged (Env env _) lastTime vars = foldM op False (IntSet.elems vars)
  -- NOTE(flupe): maybe we can early return once we reach True
  -- TODO(flupe): we shouldn't ever fail looking up the env,
  --              so we're not filtering enough variables...
  where op :: (Monad m, AchilleIO m) => Bool -> Int -> m Bool
        op b k =
          case env IntMap.!? k of
            Just var -> AIO.readMVar var >>= \case
              Just (Boxed _ t) -> pure (lastTime < t || b)
              Nothing  -> pure b
            Nothing -> pure b -- NOTE(flupe): shouldn't ever fail

depsClean :: Map Path UTCTime -> UTCTime -> DynDeps -> Bool
depsClean edits lastT (getFileDeps -> fdeps) = getAll $ foldMap (All . isClean) fdeps
  where isClean :: Path -> Bool
        isClean src = maybe False (<= lastT) (edits Map.!? src)

zeroTime :: UTCTime
zeroTime = UTCTime (toEnum 0) 0

runProgramIn
  :: (Monad m, MonadFail m, AchilleIO m)
  => Env -> Program m a -> PrimTask m (Value a)
runProgramIn env t = case t of

  Var k -> maybe halt pure =<< lift (lookupEnv env k)

  Seq x y -> do
    (cx, cy)  <- splitCache
    ctx <- ask
    mvx <- lift AIO.newEmptyMVar

    -- run x in seperate thread
    lift $ AIO.fork do
      (_, cx', deps) <- runPrimTask (runProgramIn env x) ctx cx
      AIO.putMVar mvx (cx', deps)

    -- run y without waiting for x
    (vy, cy') <- withCache cy $ runProgramIn env y

    -- now we dow wait for x
    (cx', deps) <- lift $ AIO.readMVar mvx
    tell deps

    joinCache cx' cy'
    forward vy

  -- When we bind a task to a variable
  -- we keep track of when the value *last changed*.
  -- Indeed, even if a variable did not change just now, because of conditional execution
  -- it is important to know if it changed *since the last time* a task has been executed.
  Bind x f -> do
    cached :: Maybe (UTCTime, Cache) <- fromCache
    ctx@Context{currentTime} <- ask
    let (cx, cf) = maybe (Cache.emptyCache, Cache.emptyCache)
                         (Cache.splitCache . Prelude.snd)
                         cached
        lastChange = maybe zeroTime Prelude.fst cached

    (var, env') <- lift (bindEnv env)
    mvcx <- lift AIO.newEmptyMVar

    -- fork and run x
    lift $ AIO.fork do
      res@(vx, _, _) <- runPrimTask (runProgramIn env x) ctx cx
      let lastChange' = if any hasChanged vx then currentTime else lastChange
      AIO.putMVar var (vx <&> \v -> Boxed v lastChange')
      AIO.putMVar mvcx res

    -- run f without waiting for x
    (vy, cf') <- withCache cf $ runProgramIn env' f

    -- now we do wait for x
    (vx, cx', deps) <- lift (AIO.readMVar mvcx)
    tell deps

    let lastChange' = if any hasChanged vx then currentTime else lastChange
    toCache (lastChange', Cache.joinCache cx' cf')
    forward vy
    -- TODO(flupe): propagate failure to environment
    --              to allow things that do not depend on the value to be evaluated

  Cached vs (p :: Program m a) -> do
    Context {updatedFiles, currentTime} :: Context <- ask
    cached :: Maybe (UTCTime, a, DynDeps, Cache) <- fromCache
    let (lastRun, deps, cache) :: (UTCTime, DynDeps, Cache) =
          case cached of
            Just (t, _, d, c) -> (t       , d     , c               )
            _                 -> (zeroTime, mempty, Cache.emptyCache)
    dirtyEnv <- lift (envChanged env lastRun vs)
    if  isNothing cached
     || dirtyEnv
     || not (depsClean updatedFiles lastRun deps) then do
      ((v, cache'), deps) <- listen $ withCache cache $ local (\c -> c {lastTime = lastRun}) 
                                                      $ runProgramIn env p
      case v of
        Nothing -> forward v
        Just x  -> toCache (currentTime, theVal x, deps, cache') *> forward v
    else do
      let Just (_, x, deps, _) = cached
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
        let chunks :: [(UTCTime, Cache)] = fromMaybe [] $ Cache.fromCache cfor
        (ys, chunks') <- forChanges cxs chunks
        joinCache cxs' (Cache.toCache chunks')
        forward (joinValue <$> ys)
    where
      forChanges :: [ListChange a] -> [(UTCTime, Cache)] -> PrimTask m (Maybe [ListChange b], [(UTCTime, Cache)])
      forChanges []           _     = pure (Just [], [])
      forChanges (Deleted   :vs) cs = forChanges vs (drop 1 cs) <&> first (fmap (Deleted:))
      forChanges (Inserted x:vs) cs = do
        ctx@Context{currentTime} <- ask

        (var, env') <- lift (bindEnv env)
        lift (AIO.putMVar var (Just (Boxed (value False x) zeroTime)))

        mvy <- lift AIO.newEmptyMVar

        lift $ AIO.fork do
          res <- runPrimTask (runProgramIn env' f) ctx Cache.emptyCache
          AIO.putMVar mvy res

        (changes, caches) <- forChanges vs cs
        (y, cy, deps) <- lift (AIO.readMVar mvy)
        tell deps

        case y of
          Nothing -> pure (Nothing, (zeroTime, cy) : caches)
          Just vy -> pure (fmap (Inserted (theVal vy):) changes, (currentTime, cy):caches)

      forChanges (Kept v:vs) cs = do
        ctx@Context{currentTime} <- ask

        let ((vlastChange, cv), cs') = fromMaybe ((zeroTime, Cache.emptyCache), []) (uncons cs)
        let vtchange = if hasChanged v then currentTime else vlastChange

        (var, env') <- lift (bindEnv env)
        lift (AIO.putMVar var (Just (Boxed v vtchange)))

        mvy <- lift AIO.newEmptyMVar

        lift $ AIO.fork do
          res <- runPrimTask (runProgramIn env' f) ctx cv
          AIO.putMVar mvy res

        (changes, caches) <- forChanges vs cs'
        (y, cy, deps) <- lift (AIO.readMVar mvy)
        tell deps

        case y of
          Nothing -> pure (Nothing, [(vtchange, cy)])
          Just vy -> pure (fmap (Kept vy:) changes, (vtchange, cy):caches)

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
