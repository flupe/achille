module Achille.Pandoc where

import Data.Aeson             (FromJSON)
import Data.Text              (Text)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options    (ReaderOptions, WriterOptions, def)

import Achille.IO
import Achille.Syntax
import Achille.Syntax.Core

import Achille.Pandoc.Recipe qualified as R


-- * Readers
-- 
-- $readers
--
-- Some recipes to load documents with pandoc, with or without the frontmatter header.

-- | Read a pandoc document from path, using default reader options.
readPandoc :: (Monad m, AchilleIO m) => FilePath -> Port m r Pandoc
readPandoc = readPandocWith def

-- | Read a pandoc document from path, using provided reader options.
readPandocWith
  :: (Monad m, AchilleIO m)
  => ReaderOptions -> FilePath -> Port m r Pandoc
readPandocWith ropts src = apply (R.readPandocWith ropts src) unit

-- | Read a pandoc document and its frontmatter metadata from path, using default reader options.
readPandocMeta
  :: (Monad m, AchilleIO m, FromJSON a)
  => FilePath -> Port m r (a, Pandoc)
readPandocMeta = readPandocMetaWith def

-- | Read a pandoc document and its frontmatter metadata from path, using provided reader options.
readPandocMetaWith
  :: (Monad m, AchilleIO m, FromJSON a)
  => ReaderOptions -> FilePath -> Port m r (a, Pandoc)
readPandocMetaWith ropts src = apply (R.readPandocMetaWith ropts src) unit


-- * Writers
-- 
-- $writers
--
-- Some recipes to render pandoc documents to HTML.

-- | Convert pandoc document to text, using default writer options.
renderPandoc :: Monad m => Port m r Pandoc -> Port m r Text
renderPandoc = renderPandocWith def

-- | Convert pandoc document to text, using provided writer options.
renderPandocWith :: Monad m => WriterOptions -> Port m r Pandoc -> Port m r Text
renderPandocWith wopts = apply (R.renderPandocWith wopts)


-- * Both
-- 
-- $both
--
-- Some recipes to load and render pandoc documents at once.

processPandoc :: (Monad m, AchilleIO m) => FilePath -> Port m r Text
processPandoc = processPandocWith def def

processPandocWith
  :: (Monad m, AchilleIO m) 
  => ReaderOptions -> WriterOptions -> FilePath -> Port m r Text
processPandocWith ropts wopts = renderPandocWith wopts . readPandocWith ropts

processPandocMeta
  :: (Monad m, AchilleIO m, FromJSON a)
  => FilePath -> Port m r (a, Text)
processPandocMeta = processPandocMetaWith def def

processPandocMetaWith
  :: (Monad m, AchilleIO m, FromJSON a)
  => ReaderOptions -> WriterOptions -> FilePath -> Port m r (a, Text)
processPandocMetaWith ropts wopts src = apply (R.processPandocMetaWith ropts wopts src) unit
