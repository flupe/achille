-- | Some achille utilities to load an apply Mustache templates.
module Achille.Stache 
  ( loadTemplate
  , applyTemplate
  )
  where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Data.Aeson (FromJSON)
import Data.Text.Lazy  (Text)
import Data.Aeson.Types (ToJSON)
import Text.Mustache (Template)

import Achille.IO
import Achille.Syntax
import Achille.Stache.Recipe qualified as R


-- | Load a Mustache template from file.
--   The template will be cached and restored as long as the file hasn't changed.
loadTemplate
  :: Achille task
  => task IO FilePath -> task IO Template
loadTemplate = apply R.loadTemplate


-- | Apply a Mustache template to a value.
applyTemplate
  :: (Achille task, Applicative m, ToJSON a)
  => task m Template -> task m a -> task m Text
applyTemplate t x = apply R.applyTemplate (t :*: x)
