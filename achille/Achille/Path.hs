{-# LANGUAGE DerivingStrategies #-}
module Achille.Path
  ( Path
  , toFilePath
  , (</>)
  , (-<.>)
  , takeExtension
  , takeBaseName
  , takeDirectory
  , makeRelative
  ) where

import Data.Binary
import Data.String (IsString(..))
import GHC.Generics (Generic)
import System.FilePath qualified as FP

-- NOTE(flupe): For now, Path is merely a wrapper around FilePath,
--              meant to represent filesystem paths. We do this to provide
--              specific instances (different from String).
--              In the future, we may want to actually use OsPath.
--              (Both for correctness and efficiency)

-- | A file path.
newtype Path = Path FilePath
  deriving newtype (Eq, Ord, Show, Binary)
  deriving Generic

instance IsString Path where
  fromString = Path

toFilePath :: Path -> FilePath
toFilePath (Path p) = p

-- | Combine two paths with a path separator.
(</>) :: Path -> Path -> Path
Path a </> Path b = Path (a FP.</> b)

-- | Remove the current extension and add another.
(-<.>) :: Path -> String -> Path
Path p -<.> ext = Path (p FP.-<.> ext)

takeExtension :: Path -> String
takeExtension (Path p) = FP.takeExtension p

takeBaseName :: Path -> String
takeBaseName (Path p) = FP.takeBaseName p

-- | Get the directory name, move up one level.
takeDirectory :: Path -> Path
takeDirectory (Path p) = Path (FP.takeDirectory p)

-- | Make the second path relative to the first.
makeRelative :: Path -> Path -> Path
makeRelative (Path a) (Path b) = Path (FP.makeRelative a b)
