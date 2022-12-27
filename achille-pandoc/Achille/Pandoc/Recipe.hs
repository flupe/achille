module Achille.Pandoc.Recipe where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Data.Aeson (FromJSON)
import Data.Text (Text, unpack)
import System.FilePath (takeExtension)

import Text.Pandoc.Error (renderError)
import Text.Pandoc.Class (PandocPure, PandocMonad, runPure)
import Text.Pandoc.Readers hiding (getReader)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions, WriterOptions, def)
import Text.Pandoc.Writers (writeHtml5String)

import Achille.Diffable
import Achille.IO
import Achille.Recipe

-- TODO(flupe): caching of pandoc options

-- TODO(flupe): add more readers and common extensions
--              and probably fallback with plaintext?
getReader :: String -> Maybe (ReaderOptions -> Text -> PandocPure Pandoc)
getReader ".md"       = Just readMarkdown
getReader ".markdown" = Just readMarkdown
getReader ".rst"      = Just readRST
getReader ".org"      = Just readOrg
getReader _ = Nothing


-- * Readers
-- 
-- $readers
--
-- Some recipes to load documents with pandoc, with or without the frontmatter header.

-- | Read a pandoc document from path, using default reader options.
readPandoc
  :: (MonadFail m, Applicative m, AchilleIO m)
  => Recipe m FilePath Pandoc
readPandoc = readPandocWith def

-- TODO(flupe): make this simpler by providing (Value a -> m (Value b)) -> Recipe m a b in achille
-- | Read a pandoc document from path, using provided reader options.
readPandocWith
  :: (MonadFail m, Applicative m, AchilleIO m)
  => ReaderOptions -> Recipe m FilePath Pandoc
readPandocWith ropts = (id &&& readText) >>> Recipe
  { recipeName = "readPandoc"
  , runRecipe = \ctx cache v ->
      let ((src, _), vt@(txt, _)) = splitPair v
          ext = takeExtension src
      in case getReader ext of
        Nothing -> fail $ "Could not find Pandoc reader for extension ." <> ext
        Just reader -> case runPure (reader ropts txt) of
          Left err  -> fail $ unpack $ renderError err
          Right doc -> pure (value doc (hasChanged vt), cache)
  }

-- | Read a pandoc document and its frontmatter metadata from path, using default reader options.
readPandocMeta :: (Monad m, AchilleIO m, FromJSON a) => Recipe m FilePath (a, Pandoc)
readPandocMeta = readPandocMetaWith def

-- | Read a pandoc document and its frontmatter metadata from path, using provided reader options.
readPandocMetaWith :: (Monad m, AchilleIO m, FromJSON a) => ReaderOptions -> Recipe m FilePath (a, Pandoc)
readPandocMetaWith ropts = (id &&& readText) >>> Recipe
  { recipeName = "readPandocMeta"
  , runRecipe  = \ctx cache (txt, _) -> undefined
  }


-- * Writers
-- 
-- $writers
--
-- Some recipes to render pandoc documents to HTML.

-- | Convert pandoc document to text, using default writer options.
renderPandoc :: MonadFail m => Recipe m Pandoc Text
renderPandoc = renderPandocWith def

-- | Convert pandoc document to text, using provided writer options.
renderPandocWith :: MonadFail m => WriterOptions -> Recipe m Pandoc Text
renderPandocWith wopts = Recipe
  { recipeName = "renderPandoc"
  , runRecipe = \ctx cache v@(doc, _) -> do
      case runPure (writeHtml5String wopts doc) of
        Left err -> fail $ unpack $ renderError $ err
        Right html -> pure (value html (hasChanged v), cache)
  }
