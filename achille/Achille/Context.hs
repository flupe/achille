module Achille.Context
  ( Context(..)
  ) where

import Data.Map.Strict (Map)
import Data.Time (UTCTime)

import Achille.Config (Config)
import Achille.Path (Path)


-- | Context in which tasks and recipes are run.
data Context = Context
  { lastTime     :: UTCTime          -- ^ Time of the last run.
  , currentTime  :: UTCTime
  , siteConfig   :: Config           -- ^ Site configuration
  , cleanBuild   :: Bool             -- ^ Whether to clean build and ignore change information.
  , currentDir   :: Path             -- ^ Directory used as root for glob patterns, and literal paths.
  , updatedFiles :: Map Path UTCTime -- ^ Files that are known to be dynamic dependencies
                                     --   and for which we have looked up the last modification time.
  , verbose      :: Bool             -- ^ Whether to print debug statements.
  , colorful     :: Bool             -- ^ Whether to colorize logs.
  }

