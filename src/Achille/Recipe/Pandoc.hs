{-# LANGUAGE RecordWildCards #-}

module Achille.Recipe.Pandoc
    ( readPandoc
    , readPandocWith
    , readPandocMetadata
    , readPandocMetadataWith
    , renderPandoc
    , renderPandocWith
    , compilePandoc
    , compilePandocWith
    ) where

import Data.Binary      (Binary, encodeFile)
import Data.Functor     (void)
import Data.Text        (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (copyFile, createDirectoryIfMissing, withCurrentDirectory)
import Text.Blaze.Html  (Html)
import System.FilePath
import Text.Pandoc      hiding (nonCached)
import Data.Aeson.Types (FromJSON)
import Data.Frontmatter (parseYamlFrontmatter, IResult(..))

import qualified Data.Text.IO                     as Text
import qualified System.FilePath.Glob             as Glob
import qualified Data.ByteString                  as ByteString
import qualified Data.ByteString.Lazy             as LazyByteString
import qualified Text.Blaze.Html.Renderer.String  as BlazeString
import qualified System.FilePath                  as Path
import qualified System.Process                   as Process


import           Achille.Config
import           Achille.Internal hiding (currentDir)
import qualified Achille.Internal as Internal
import           Achille.Recipe


-- | Recipe for loading a pandoc document
readPandoc :: Recipe FilePath Pandoc
readPandoc = readPandocWith def

-- | Recipe for loading a pandoc document using a given reader config
readPandocWith :: ReaderOptions -> Recipe FilePath Pandoc
readPandocWith ropts = nonCached \Context{..}  ->
    let ext = drop 1 $ takeExtension inputValue
        Just reader = lookup (pack ext) readers
    in case reader of
        ByteStringReader f ->
            LazyByteString.readFile (inputDir </> currentDir </> inputValue)
                >>= runIOorExplode <$> f ropts
        TextReader f ->
            Text.readFile (inputDir </> currentDir </> inputValue)
                >>= runIOorExplode <$> f ropts

-- | Recipe for loading a pandoc document and a frontmatter header.
readPandocMetadata :: FromJSON a => Recipe FilePath (a, Pandoc)
readPandocMetadata = readPandocMetadataWith def

-- | Recipe for loading a pandoc document using a given reader config
readPandocMetadataWith :: FromJSON a => ReaderOptions -> Recipe FilePath (a, Pandoc)
readPandocMetadataWith ropts = nonCached \Context{..} -> do
    let ext         = drop 1 $ takeExtension inputValue
        Just reader = lookup (pack ext) readers
    contents <- ByteString.readFile (inputDir </> currentDir </> inputValue)
    (meta, remaining) <-
            case parseYamlFrontmatter contents of
                Done i a -> pure (a, i)
                _        -> fail $ "error while loading meta of " <> inputValue
    (meta,) <$> case reader of
        ByteStringReader f -> runIOorExplode $ f ropts (LazyByteString.fromStrict remaining)
        TextReader f -> runIOorExplode $ f ropts (decodeUtf8 remaining)

-- | Recipe to convert a Pandoc document to HTML.
renderPandoc :: Pandoc -> Recipe a Html
renderPandoc = renderPandocWith def 

-- | Recipe to convert a Pandoc document to HTML using specified writer options.
renderPandocWith :: WriterOptions -> Pandoc -> Recipe a Html
renderPandocWith wopts = liftIO . runIOorExplode <$> writeHtml5 wopts

-- | Recipe to load and convert a Pandoc document to HTML.
compilePandoc :: Recipe FilePath Html
compilePandoc = readPandoc >>= renderPandoc

-- | Recipe to load and convert a Pandoc document to HTML.
compilePandocWith :: ReaderOptions -> WriterOptions -> Recipe FilePath Html
compilePandocWith ropts wopts =
    readPandocWith ropts >>= renderPandocWith wopts
