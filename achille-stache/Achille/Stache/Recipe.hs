{-# LANGUAGE StandaloneDeriving #-}
module Achille.Stache.Recipe where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Data.Binary (Binary)
import Data.Aeson (ToJSON(toJSON))
import Data.Text.Lazy (Text)
import Data.Map.Strict (Map)
import System.FilePath (takeExtension)
import Text.Mustache
import Text.Megaparsec.Pos (Pos)

import Achille.Diffable
import Achille.IO
import Achille.Recipe


-- standalone instances
deriving instance Binary PName
deriving instance Binary Key
deriving instance Binary Pos
deriving instance Binary Node
deriving instance Binary Template


loadTemplate :: Recipe IO FilePath Template
loadTemplate = undefined


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
