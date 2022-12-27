-- | Some achille utilities to process pandoc documents.
module Achille.Pandoc where

import Data.Aeson             (FromJSON)
import Data.Text              (Text)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options    (ReaderOptions, WriterOptions, def)

import Achille.IO
import Achille.Syntax
import Achille.Pandoc.Recipe qualified as R


-- * Readers
-- 
-- $readers
--
-- Some recipes to load documents with pandoc, with or without the frontmatter header.

-- | Read a pandoc document from path, using default reader options.
readPandoc
  :: (Achille task, Monad m, AchilleIO m)
  => task m FilePath -> task m Pandoc
readPandoc = readPandocWith def

-- | Read a pandoc document from path, using provided reader options.
readPandocWith
  :: (Achille task, Monad m, AchilleIO m)
  => ReaderOptions -> task m FilePath -> task m Pandoc
readPandocWith ropts src = apply (R.readPandocWith ropts) src

-- | Read a pandoc document and its frontmatter metadata from path, using default reader options.
readPandocMeta
  :: (Achille task, Monad m, AchilleIO m, FromJSON a)
  => task m FilePath -> task m (a, Pandoc)
readPandocMeta = readPandocMetaWith def

-- | Read a pandoc document and its frontmatter metadata from path, using provided reader options.
readPandocMetaWith
  :: (Achille task, Monad m, AchilleIO m, FromJSON a)
  => ReaderOptions -> task m FilePath -> task m (a, Pandoc)
readPandocMetaWith ropts src = apply (R.readPandocMetaWith ropts) src


-- * Writers
-- 
-- $writers
--
-- Some recipes to render pandoc documents to HTML.

-- | Convert pandoc document to text, using default writer options.
renderPandoc
  :: (Achille task, Monad m)
  => task m Pandoc -> task m Text
renderPandoc = renderPandocWith def

-- | Convert pandoc document to text, using provided writer options.
renderPandocWith
  :: (Achille task, Monad m)
  => WriterOptions -> task m Pandoc -> task m Text
renderPandocWith wopts = apply (R.renderPandocWith wopts)


-- * Both
-- 
-- $both
--
-- Some recipes to load and render pandoc documents at once.

-- | Read a pandoc document from path using default reader options,
--   and convert to text using default writer options.
processPandoc
  :: (Achille task, Monad m, AchilleIO m)
  => task m FilePath -> task m Text
processPandoc = processPandocWith def def

-- | Read a pandoc document from path and convert to text,
--   using the provided reader and writer options.
processPandocWith
  :: (Achille task, Monad m, AchilleIO m) 
  => ReaderOptions -> WriterOptions -> task m FilePath -> task m Text
processPandocWith ropts wopts = renderPandocWith wopts . readPandocWith ropts

-- | Read a pandoc document and its frontmatter metadata from path
--   using default reader options, and convert document to text using default
--   writer options.
processPandocMeta
  :: (Achille task, Monad m, AchilleIO m, FromJSON a)
  => task m FilePath -> task m (a, Text)
processPandocMeta = processPandocMetaWith def def

-- | Read a pandoc document and its frontmatter metadata from path,
--   and convert document to text using the provided reader and writer
--   options.
processPandocMetaWith
  :: (Achille task, Monad m, AchilleIO m, FromJSON a)
  => ReaderOptions -> WriterOptions -> task m FilePath -> task m (a, Text)
processPandocMetaWith ropts wopts src = apply (R.processPandocMetaWith ropts wopts) src
