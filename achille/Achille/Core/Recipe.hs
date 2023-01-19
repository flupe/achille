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
import Achille.Result


type PrimRecipe m a b = Cache -> Value a -> Result m (Value b, Cache)

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


runRecipe :: Monad m => Recipe m a b -> PrimRecipe m a b
runRecipe r cache x = case r of
  Id -> pure (x, cache)

  Comp g f -> do
    let (cf, cg) = splitCache cache
    (y, cf) <- runRecipe f cf x
    (z, cg) <- runRecipe g cg y
    pure (z, joinCache cf cg)

  f :***: g -> do -- TODO(flupe): parallelism
    let (cf, cg) = splitCache cache
        (a, b) = splitValue x
    (a, cf) <- runRecipe f cf a
    (b, cg) <- runRecipe g cg b
    pure (joinValue (a, b), joinCache cf cg)

  f :&&&: g -> do -- TODO(flupe): parallelism
    let (cf, cg) = splitCache cache
    (a, cf) <- runRecipe f cf x
    (b, cg) <- runRecipe g cg x
    pure (joinValue (a, b), joinCache cf cg)

  Exl  -> pure (fst (splitValue x), cache)
  Exr  -> pure (snd (splitValue x), cache)
  Void -> pure (unit, cache)

  Embed n r -> r cache x
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
  arr f = recipe "Achille.Core.Recipe.arr" \cache x -> pure (f <$> x, cache)
  {-# INLINE arr #-}
  first f = f :***: id
  {-# INLINE first #-}
  second f = id :***: f
  {-# INLINE second #-}
  (***) = (:***:)
  {-# INLINE (***) #-}
  (&&&) = (:&&&:)
  {-# INLINE (&&&) #-}
