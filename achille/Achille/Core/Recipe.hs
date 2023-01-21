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
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async as Async

import Data.Set qualified as Set
import System.FilePath.Glob qualified as Glob

import Achille.Path
import Achille.Diffable
import Achille.IO
import Achille.Task.Prim
import Achille.Cache qualified as Cache


type PrimRecipe m a b = Value a -> PrimTask m (Value b)

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
  Embed   :: !Text -> !(PrimRecipe m a b) -> Recipe m a b

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
recipe = Embed
{-# INLINE recipe #-}


runRecipe :: (Monad m, MonadUnliftIO m) => Recipe m a b -> PrimRecipe m a b
runRecipe r x = case r of
  Id -> pure x

  Comp g f -> do
    (cf, cg) <- splitCache
    (y, cf) <- withCache cf $ runRecipe f x
    case y of
      Nothing -> toCache (cf, cg) *> halt
      Just y  -> do
        (z, cg) <- withCache cg $ runRecipe g y
        joinCache cf cg
        forward z

  f :***: g -> prim \ctx cache -> do
    let (a, b)   = splitValue x
    let (cf, cg) = Cache.splitCache cache

    one <- async $ runPrimTask (runRecipe f a) ctx cf
    two <- async $ runPrimTask (runRecipe g b) ctx cg
    ((a, cf, adeps), (b, cg, bdeps)) <- waitBoth one two

    pure ( joinValue <$> ((,) <$> a <*> b)
         , Cache.joinCache cf cg
         , adeps <> bdeps
         )

  f :&&&: g -> prim \ctx cache -> do
    let (cf, cg) = Cache.splitCache cache

    one <- async $ runPrimTask (runRecipe f x) ctx cf
    two <- async $ runPrimTask (runRecipe g x) ctx cg
    ((a, cf, adeps), (b, cg, bdeps)) <- waitBoth one two

    pure ( joinValue <$> ((,) <$> a <*> b)
         , Cache.joinCache cf cg
         , adeps <> bdeps
         )

  Exl  -> pure $ fst (splitValue x)
  Exr  -> pure $ snd (splitValue x)
  Void -> pure unit

  Embed n r -> r x


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


instance Monad m => Arrow (Recipe m) where
  -- TODO(flupe): maybe make some smart constructors applying category laws
  arr f = recipe "Achille.Core.Recipe.arr" (pure . fmap f)
  {-# INLINE arr #-}
  first f = f :***: id
  {-# INLINE first #-}
  second f = id :***: f
  {-# INLINE second #-}
  (***) = (:***:)
  {-# INLINE (***) #-}
  (&&&) = (:&&&:)
  {-# INLINE (&&&) #-}
