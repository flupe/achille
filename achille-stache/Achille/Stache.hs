-- | Some achille utilities to load an apply Mustache templates.
module Achille.Stache 
  ( loadTemplate
  , loadTemplates
  , applyTemplate
  )
  where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Data.Aeson (FromJSON)
import Data.Text.Lazy  (Text)
import Data.Map.Strict (Map)
import Data.Aeson.Types (ToJSON)
import Text.Mustache (Template, PName)

import Achille.IO
import Achille.Path
import Achille.Task
import Achille.Stache.Recipe qualified as R


-- | Load a Mustache template from file.
--   The template will be cached and restored as long as the file hasn't changed.
loadTemplate :: Task IO Path -> Task IO Template
loadTemplate = apply R.loadTemplate

-- | Load all the templates in the given directory.
loadTemplates :: Task IO Path -> Task IO (Map PName Template)
loadTemplates = apply R.loadTemplates

-- | Apply a Mustache template to a value.
applyTemplate
  :: (Monad m, ToJSON a)
  => Task m Template -> Task m a -> Task m Text
applyTemplate t x = apply R.applyTemplate (t :*: x)
