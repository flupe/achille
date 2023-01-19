module Achille.DynDeps
  ( DynDeps
  , getFileDeps
  , getGlobDeps
  , dependsOnFile
  , dependsOnFiles
  , dependsOnPattern
  ) where

import Data.Binary (Binary(get, put))
import Data.List (nub)
import Data.Set (Set)
import System.FilePath.Glob (Pattern)

import Data.Binary qualified as Binary
import Data.Set qualified as Set
import System.FilePath.Glob qualified as Glob

import Achille.Path (Path)

-- NOTE(flupe): should a file that doesn't exist anymore but a dynamic dependency be reported when we
--              check all dependencies at startup? or should let the build system proceed to the place 
--              where it's needed, and let it fail here?
--              probably the latter

-- | Representation of dynamic dependencies on the file system.
data DynDeps = Deps
  { getFileDeps :: Set Path
    -- NOTE(flupe): ^ Maybe we want to make this `Map FilePath UTCTime`? (to handle failures gracefully)
  , getGlobDeps :: [Pattern]
  } deriving (Show)


instance Semigroup DynDeps where
  Deps fs1 !g1 <> Deps fs2 !g2 =
    Deps (fs1 <> fs2) (g1 <> g2)

instance Monoid DynDeps where
  mempty = Deps mempty mempty

instance Binary DynDeps where
  get = Deps <$> Binary.get <*> (fmap Glob.compile <$> Binary.get)
  put (Deps files pat) =
       Binary.put files
    *> Binary.put (nub $ fmap Glob.decompile pat)


-- | Express dynamic dependency on a single file.
dependsOnFile :: Path -> DynDeps
dependsOnFile file = Deps (Set.singleton file) mempty

-- | Express dynamic dependency on a list of files.
dependsOnFiles :: [Path] -> DynDeps
dependsOnFiles files = Deps (Set.fromList files) mempty

-- | Express dynamic dependency over files matching a glob pattern.
dependsOnPattern :: Pattern -> DynDeps
dependsOnPattern pat = Deps mempty [pat]

