module Achille.Pandoc.Recipe where

import Prelude hiding ((.))

import Control.Category       ((.))
import Data.Aeson             (FromJSON)
import Data.Text              (Text)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options    (ReaderOptions, WriterOptions, def)

import Achille.IO
import Achille.Recipe


-- * Readers
-- 
-- $readers
--
-- Some recipes to load documents with pandoc, with or without the frontmatter header.

-- | Read a pandoc document from path, using default reader options.
readPandoc :: AchilleIO m => FilePath -> Recipe m i Pandoc
readPandoc = readPandocWith def

-- | Read a pandoc document from path, using provided reader options.
readPandocWith :: AchilleIO m => ReaderOptions -> FilePath -> Recipe m i Pandoc
readPandocWith ropts = undefined

-- | Read a pandoc document and its frontmatter metadata from path, using default reader options.
readPandocMeta :: (AchilleIO m, FromJSON a) => FilePath -> Recipe m i (a, Pandoc)
readPandocMeta = readPandocMetaWith def

-- | Read a pandoc document and its frontmatter metadata from path, using provided reader options.
readPandocMetaWith :: (AchilleIO m, FromJSON a) => ReaderOptions -> FilePath -> Recipe m i (a, Pandoc)
readPandocMetaWith ropts = undefined


-- * Writers
-- 
-- $writers
--
-- Some recipes to render pandoc documents to HTML.

-- | Convert pandoc document to text, using default writer options.
renderPandoc :: Recipe m Pandoc Text
renderPandoc = renderPandocWith def

-- | Convert pandoc document to text, using provided writer options.
renderPandocWith :: WriterOptions -> Recipe m Pandoc Text
renderPandocWith wopts = undefined


-- * Both
-- 
-- $both
--
-- Some recipes to load and render pandoc documents at once.

processPandoc :: (Monad m, AchilleIO m) => FilePath -> Recipe m i Text
processPandoc = processPandocWith def def

processPandocWith :: (Monad m, AchilleIO m) => ReaderOptions -> WriterOptions -> FilePath -> Recipe m a Text
processPandocWith ropts wopts src = renderPandocWith wopts . readPandocWith ropts src

processPandocMeta :: (AchilleIO m, FromJSON a) => FilePath -> Recipe m i (a, Text)
processPandocMeta = undefined

processPandocMetaWith :: (AchilleIO m, FromJSON a) => ReaderOptions -> WriterOptions -> FilePath -> Recipe m i (a, Text)
processPandocMetaWith ropts wopts = undefined

