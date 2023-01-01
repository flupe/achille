module Achille.Core.Recipe where

import Prelude hiding ((.), id, seq, fail)

import Control.Category
import Control.Arrow

import Data.Time (UTCTime)
import System.FilePath ((</>))
import System.FilePath.Glob (Pattern)

import Achille.Cache
import Achille.Diffable
import Achille.IO


-- | Context in which tasks and recipes are run.
data Context = Context
  { lastTime   :: UTCTime  -- ^ Time of the last run.
  , currentDir :: FilePath -- ^ Current directory used for glob patterns
  , inputRoot  :: FilePath
  , outputRoot :: FilePath
  , sitePrefix :: FilePath
  }

type PrimRecipe m a b = Context -> Cache -> Value a -> m (Value b, Cache) 

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
  Embed   :: !String -> !(PrimRecipe m a b) -> Recipe m a b

instance Show (Recipe m a b) where
  show Id = "Id"
  show (Comp g f) = "Comp (" <> show g <> ") (" <> show f <> ")"
  show (f :***: g) = "(" <> show f <> ") :***: (" <> show g <> ")"
  show (f :&&&: g) = "(" <> show f <> ") :&&&: (" <> show g <> ")"
  show Exl = "Exl"
  show Exr = "Exr"
  show Void = "Void"
  show (Embed e r) = "Embed " <> show e


recipe :: String -> PrimRecipe m a b -> Recipe m a b
recipe = Embed
{-# INLINE recipe #-}


runRecipe :: Monad m => Recipe m a b -> PrimRecipe m a b
runRecipe r ctx cache v = case r of
  Id -> pure (v, cache)

  Comp g f -> do
    let (cf, cg) = splitCache cache
    (y, cf) <- runRecipe f ctx cf v
    (z, cg) <- runRecipe g ctx cg y
    pure (z, joinCache cf cg)

  f :***: g -> do -- TODO(flupe): parallelism
    let (cf, cg) = splitCache cache
        (x, y) = splitPair v
    (z, cf) <- runRecipe f ctx cf x
    (w, cg) <- runRecipe g ctx cg y
    pure (joinPair z w, joinCache cf cg)

  f :&&&: g -> do -- TODO(flupe): parallelism
    let (cf, cg) = splitCache cache
    (z, cf) <- runRecipe f ctx cf v
    (w, cg) <- runRecipe g ctx cg v
    pure (joinPair z w, joinCache cf cg)

  Exl  -> pure (fst $ splitPair v, cache)
  Exr  -> pure (snd $ splitPair v, cache)
  Void -> pure (unit, cache)

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
  arr f = recipe "arr" \ctx cache v@(x, _) -> pure (value (f x) (hasChanged v), cache)
  {-# INLINE arr #-}
  first f = f :***: id
  {-# INLINE first #-}
  second f = id :***: f
  {-# INLINE second #-}
  (***) = (:***:)
  {-# INLINE (***) #-}
  (&&&) = (:&&&:)
  {-# INLINE (&&&) #-}

