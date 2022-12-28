{-# LANGUAGE StandaloneDeriving, RecordWildCards #-}
module Achille.Stache.Recipe where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Data.Binary (Binary)
import Data.Aeson (ToJSON(toJSON))
import Data.Text.Lazy (Text)
import Data.Map.Strict (Map)
import System.FilePath ((</>))
import Text.Mustache
import Text.Megaparsec.Pos (Pos)

import Achille.Cache
import Achille.Diffable
import Achille.IO
import Achille.Recipe


-- standalone instances
deriving instance Binary PName
deriving instance Binary Key
deriving instance Binary Pos
deriving instance Binary Node
deriving instance Binary Template

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b x = b >>= \case
  True  -> pure ()
  False -> x


loadTemplate :: Recipe IO FilePath Template
loadTemplate = embed Embedded
  { rName = "loadTemplate"
  , runEmbed = \Context{..} cache v@(src, _) -> do
      let path = inputRoot </> src -- TODO(flupe): whether to add currentDir or not is problematic
      unlessM (doesFileExist path) $ fail $ "Could not find template file " <> path
      mtime <- getModificationTime path
      case fromCache cache of
        Just (t :: Template) | mtime <= lastTime, not (hasChanged v) -> pure (value t False, cache)
        _ -> do
          t <- compileMustacheFile path -- TODO(flupe): handle parser exception gracefully
          pure (value t True, toCache t)
  }


loadTemplates :: Recipe IO FilePath (Map PName Template)
loadTemplates = undefined


applyTemplate
  :: (Applicative m, ToJSON a)
  => Recipe m (Template, a) Text
applyTemplate = embed Embedded
  { rName = "applyTemplate"
  , runEmbed = \ctx cache v@((t, x), _) ->
      pure (value (renderMustache t $ toJSON x) (hasChanged v), cache)
  }
