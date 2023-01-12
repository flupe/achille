-- | Some achille utilities to process pandoc documents.
module Achille.Pandoc where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Data.Aeson             (FromJSON)
import Data.Text              (Text)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options    (ReaderOptions(readerExtensions), WriterOptions, def)
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Class (PandocMonad)

import Achille.IO
import Achille.Task
import Achille.Pandoc.Recipe qualified as R
import Achille.Path


-- * Readers
-- 
-- $readers
--
-- Some recipes to load documents with pandoc, with or without the frontmatter header.

-- | Read a pandoc document from path, using default reader options.
readPandoc
  :: (MonadFail m, AchilleIO m)
  => Task m Path -> Task m Pandoc
readPandoc = readPandocWith def { readerExtensions = pandocExtensions }

-- | Read a pandoc document from path, using provided reader options.
readPandocWith
  :: (MonadFail m, AchilleIO m)
  => ReaderOptions -> Task m Path -> Task m Pandoc
readPandocWith ropts src = apply (R.readPandocWith ropts) src

-- | Read a pandoc document and its frontmatter metadata from path, using default reader options.
readPandocMeta
  :: (MonadFail m, AchilleIO m, FromJSON a)
  => Task m Path -> Task m (a, Pandoc)
readPandocMeta = readPandocMetaWith def

-- | Read a pandoc document and its frontmatter metadata from path, using provided reader options.
readPandocMetaWith
  :: (MonadFail m, AchilleIO m, FromJSON a)
  => ReaderOptions -> Task m Path -> Task m (a, Pandoc)
readPandocMetaWith ropts src = apply (R.readPandocMetaWith ropts) src


-- * Writers
-- 
-- $writers
--
-- Some recipes to render pandoc documents to HTML.

-- | Convert pandoc document to text, using default writer options.
renderPandoc :: MonadFail m => Task m Pandoc -> Task m Text
renderPandoc = renderPandocWith def

-- | Convert pandoc document to text, using provided writer options.
renderPandocWith :: MonadFail m => WriterOptions -> Task m Pandoc -> Task m Text
renderPandocWith wopts = apply (R.renderPandocWith wopts)


-- * Both
-- 
-- $both
--
-- Some recipes to load and render pandoc documents at once.

-- | Read a pandoc document from path using default reader options,
--   and convert to text using default writer options.
processPandoc :: (MonadFail m, AchilleIO m) => Task m Path -> Task m Text
processPandoc = processPandocWith def {readerExtensions = pandocExtensions} def

-- | Read a pandoc document from path and convert to text,
--   using the provided reader and writer options.
processPandocWith
  :: (MonadFail m, AchilleIO m)
  => ReaderOptions -> WriterOptions -> Task m Path -> Task m Text
processPandocWith ropts wopts = renderPandocWith wopts . readPandocWith ropts

-- | Read a pandoc document and its frontmatter metadata from path
--   using default reader options, and convert document to text using default
--   writer options.
processPandocMeta
  :: (MonadFail m, AchilleIO m, FromJSON a)
  => Task m Path -> Task m (a, Text)
processPandocMeta = processPandocMetaWith def {readerExtensions = pandocExtensions} def

-- | Read a pandoc document and its frontmatter metadata from path,
--   and convert document to text using the provided reader and writer
--   options.
processPandocMetaWith
  :: (MonadFail m, AchilleIO m, FromJSON a)
  => ReaderOptions -> WriterOptions -> Task m Path -> Task m (a, Text)
processPandocMetaWith ropts wopts src =
  apply ((id *** R.renderPandocWith wopts) . R.readPandocMetaWith ropts) src
