{-# LANGUAGE InstanceSigs, DeriveGeneric, DeriveAnyClass, TypeFamilies, Rank2Types, TypeOperators, ScopedTypeVariables, FlexibleContexts #-}

-- | Core recipe definition.
module Achille.Recipe where

import Prelude
import Data.Map hiding ((\\))
import Data.Binary
import Data.ByteString.Lazy as LBS
import GHC.Generics
import qualified Data.Binary as Binary
import Control.Category.Constrained
import Data.Constraint
import Data.Constraint.Deferrable

-- | The cache received by recipes. It is a list in order to make composition associative.
newtype Cache = Cache { chunks :: [ByteString] } deriving (Generic, Binary)

-- | Context in which a recipe is run.
data Context = Context
  { tagged :: Map String Cache -- ^ A map of caches, used for expensive computations that 
                               -- should be made insensitive to code refactoring
  }

-- | The recipe abstraction.
data Recipe m a b = Recipe { runRecipe :: Context -> Cache -> a -> m (b, Cache) }


emptyCache :: Cache
emptyCache = Cache []


-- | Splits the cache in two.
splitCache :: Cache -> (Cache, Cache)
splitCache (Cache []) = (emptyCache, emptyCache)
splitCache (Cache (c:cs)) = (Binary.decode c, Cache cs)


-- | Combines two caches.
joinCache :: Cache -> Cache -> Cache
joinCache hd (Cache cs) = Cache (Binary.encode hd : cs)


-- | Class for things that have a diff
class Diffable a where
  type Diff a
  type Diff a = Bool

  subDiffable :: forall b c. (Diffable (b, c), a ~ (b, c)) => Dict (Diffable b, Diffable c)
  subDiffable = error "not a product"

instance Diffable () where
  type Diff () = ()

instance (Diffable a, Diffable b) => Diffable (a, b) where
  type Diff (a, b) = (Diff a, Diff b)
  subDiffable = Dict


instance ProdObj Diffable where
  objprod :: forall z a b. (z ~ (a, b), Diffable z) => Dict (Diffable a, Diffable b)
  objprod = subDiffable

  prodobj :: (Diffable a, Diffable b) => Dict (Diffable (a, b))
  prodobj = Dict

  objunit :: Dict (Diffable ())
  objunit = Dict


instance Monad m => Category (Recipe m) where
  type Obj (Recipe m) = Diffable

  id :: Diffable a => Recipe m a a
  id = Recipe \ctx cache x -> pure (x, cache)

  (∘) :: (Diffable a, Diffable b, Diffable c) => Recipe m b c -> Recipe m a b -> Recipe m a c
  Recipe g ∘ Recipe f = Recipe \ctx cache x -> do
    let (cf, cg) = splitCache cache
    (y, cf') <- f ctx cf x
    (z, cg') <- g ctx cg y
    pure (z, joinCache cf' cg')


instance Monad m => Monoidal (Recipe m) where
  (×) :: (Diffable a, Diffable b, Diffable c, Diffable d)
      => Recipe m a b -> Recipe m c d -> Recipe m (a ⊗ c) (b ⊗ d)
  Recipe f × Recipe g = undefined

  swap :: (Diffable a, Diffable b) => Recipe m (a ⊗ b) (b ⊗ a)
  swap = Recipe \ctx cache (x, y) -> pure ((y, x), cache)

  assoc :: (Diffable a, Diffable b, Diffable c) => Recipe m ((a ⊗ b) ⊗ c) (a ⊗ (b ⊗ c))
  assoc = Recipe \ctx cache ((x, y), z) -> pure ((x, (y, z)), cache)

  assoc' :: (Diffable a, Diffable b, Diffable c) => Recipe m (a ⊗ (b ⊗ c)) ((a ⊗ b) ⊗ c)
  assoc' = Recipe \ctx cache (x, (y, z)) -> pure (((x, y), z), cache)

  unitor :: Diffable a => Recipe m a (a ⊗ ())
  unitor = Recipe \ctx cache x -> pure ((x, ()), cache)

  unitor' :: Diffable a => Recipe m (a ⊗ ()) a
  unitor' = Recipe \ctx cache (x, ()) -> pure (x, cache)
