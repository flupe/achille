{-# LANGUAGE OverloadedStrings #-}
module Achille.Core.Recipe where

import Prelude hiding ((.), id, seq, fail)

import Control.Category
import Control.Arrow
import Data.Binary (Binary(..))
import Data.List (nub)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Set (Set)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import System.FilePath.Glob (Pattern)

import Data.Set qualified as Set
import System.FilePath.Glob qualified as Glob

import Achille.Path
import Achille.Cache
import Achille.Diffable
import Achille.IO


data FileDeps = Deps
  { getFileDeps :: Set Path
    -- NOTE(flupe): ^ Maybe we want to make this `Map FilePath UTCTime`? (to handle failures gracefully)
  , getGlobDeps :: [Pattern]
  } deriving (Show)

instance Semigroup FileDeps where
  Deps fs1 !g1 <> Deps fs2 !g2 = Deps (fs1 <> fs2) (g1 <> g2)

instance Monoid FileDeps where
  mempty = Deps mempty mempty

instance Binary FileDeps where
  get = Deps <$> get <*> (fmap Glob.compile <$> get)
  put (Deps files pat) =
       put files
    *> put (nub $ fmap Glob.decompile pat)

dependsOnFiles :: [Path] -> FileDeps
dependsOnFiles files = Deps (Set.fromList files) mempty

dependsOnFile :: Path -> FileDeps
dependsOnFile file = Deps (Set.singleton file) mempty

dependsOnPattern :: Pattern -> FileDeps
dependsOnPattern pat = Deps mempty [pat]


-- | Context in which tasks and recipes are run.
data Context = Context
  { lastTime     :: UTCTime -- ^ Time of the last run.
  , cleanBuild   :: Bool    -- ^ Whether to clean build and ignore change information.
  , currentDir   :: Path    -- ^ Directory used as root for glob patterns, and literal paths.
  , inputRoot    :: Path
  , outputRoot   :: Path
  , updatedFiles :: Map Path UTCTime -- ^ Files that are known to be dynamic dependencies
                                         --   and for which we have looked up the last modification time.
  , sitePrefix   :: Text
  }

-- re ^ updatedFiles
-- NOTE(flupe): should a file that doesn't exist anymore but a dynamic dependency be reported when we
--              check all dependencies at startup? or should let the build system proceed to the place 
--              where it's needed, and let it fail here?
--              probably the latter

data Result b = Result
  { output   :: Value b
  , fileDeps :: FileDeps
  , newCache :: Cache
  }

type PrimRecipe m a b = Context -> Cache -> Value a -> m (Value b, Cache)
type PrimRecipeDyn m a b = Context -> Cache -> Value a -> m (Result b)

toDyn :: (Value b, Cache) -> Result b
toDyn (v, c) = Result v mempty c

-- | A recipe is a glorified Kleisli arrow, computing a value of type @b@ in some monad @m@
--   given some input of type @a@ and a context. The recipe has access to a local cache,
--   preserved between runs.
data Recipe m a b where
  Id      :: Recipe m a a
  Comp    :: Recipe m b c -> Recipe m a b -> Recipe m a c
  (:***:) :: Recipe m a b -> Recipe m c d -> Recipe m (a, c) (b, d)
  (:&&&:) :: Recipe m a b -> Recipe m a c -> Recipe m a (b, c)
  Exl     :: Recipe m (a, b) a
  Exr     :: Recipe m (a, b) b
  Void    :: Recipe m a ()
  Embed   :: !Text -> !(PrimRecipeDyn m a b) -> Recipe m a b

instance Show (Recipe m a b) where
  show Id = "Id"
  show (Comp g f) = "Comp (" <> show g <> ") (" <> show f <> ")"
  show (f :***: g) = "(" <> show f <> ") :***: (" <> show g <> ")"
  show (f :&&&: g) = "(" <> show f <> ") :&&&: (" <> show g <> ")"
  show Exl = "Exl"
  show Exr = "Exr"
  show Void = "Void"
  show (Embed e r) = "Embed " <> show e


-- TODO(flupe): clean this up?
recipe :: Functor m => Text -> PrimRecipe m a b -> Recipe m a b
recipe s r = Embed s (\ctx cache v -> toDyn <$> r ctx cache v)
{-# INLINE recipe #-}

recipeDyn :: Functor m => Text -> PrimRecipeDyn m a b -> Recipe m a b
recipeDyn = Embed
{-# INLINE recipeDyn #-}

runRecipe :: Monad m => Recipe m a b -> PrimRecipeDyn m a b
runRecipe r ctx cache v = case r of
  Id -> pure $ Result v mempty cache

  Comp g f -> do
    let (cf, cg) = splitCache cache
    Result y df cf <- runRecipe f ctx cf v
    Result z dg cg <- runRecipe g ctx cg y
    pure $ Result z (df <> dg) (joinCache cf cg)

  f :***: g -> do -- TODO(flupe): parallelism
    let (cf, cg) = splitCache cache
        (vx, vy) = splitValue v
    Result vz df cf <- runRecipe f ctx cf vx
    Result vw dg cg <- runRecipe g ctx cg vy
    pure $ Result (joinValue (vz, vw)) (df <> dg) (joinCache cf cg)

  f :&&&: g -> do -- TODO(flupe): parallelism
    let (cf, cg) = splitCache cache
    Result vz df cf <- runRecipe f ctx cf v
    Result vw dg cg <- runRecipe g ctx cg v
    pure $ Result (joinValue (vz, vw)) (df <> dg) (joinCache cf cg)

  Exl  -> pure $ Result (fst $ splitValue v) mempty cache
  Exr  -> pure $ Result (snd $ splitValue v) mempty cache
  Void -> pure $ Result unit mempty cache

  Embed n r -> r ctx cache v
{-# INLINE runRecipe #-}


instance Category (Recipe m) where
  id = Id
  {-# INLINE id #-}

  -- simplify morphisms using category laws
  Comp g f    . h           = g . (f . h) -- right-nested composition chain
  Id          . f           = f
  g           . Id          = g
  (p :***: q) . (f :***: g) = (p . f) :***: (q . g)
  (p :***: q) . (f :&&&: g) = (p . f) :&&&: (q . g)
  -- NOTE(flupe): the commented rules should *never* be applied. while they hold for cartesian categories, 
  --              in our case even if output is ignored, we still want to "execute the morphisms"
  -- Exl         . (f :&&&: g) = f
  -- Exr         . (f :&&&: g) = g
  -- Exl         . (f :***: g) = f . Exl
  -- Exr         . (f :***: g) = g . Exr
  g           . f           = Comp g f
  {-# INLINE (.) #-}


instance Applicative m => Arrow (Recipe m) where
  -- TODO(flupe): maybe make some smart constructors applying category laws
  arr f = recipe "Achille.Core.Recipe.arr" \ctx cache v -> pure (f <$> v, cache)
  {-# INLINE arr #-}
  first f = f :***: id
  {-# INLINE first #-}
  second f = id :***: f
  {-# INLINE second #-}
  (***) = (:***:)
  {-# INLINE (***) #-}
  (&&&) = (:&&&:)
  {-# INLINE (&&&) #-}
