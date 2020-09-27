{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines convenience recipes for reading and writing documents with pandoc.
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary      (Binary, encodeFile)
import Data.Functor     (void)
import Data.Text        (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (copyFile, createDirectoryIfMissing, withCurrentDirectory)

import System.FilePath
import Text.Pandoc      hiding (nonCached)
import Data.Aeson.Types (FromJSON)
import Data.Frontmatter (parseYamlFrontmatter, IResult(..))

import qualified Data.Text.IO                     as Text
import qualified System.FilePath.Glob             as Glob
import qualified Data.ByteString                  as ByteString
import qualified Data.ByteString.Lazy             as LazyByteString
import qualified System.FilePath                  as Path
import qualified System.Process                   as Process

import           Achille.Config
import           Achille.Internal hiding (currentDir)
import qualified Achille.Internal as Internal
import           Achille.Recipe
import           Achille.Writable as Writable
import           Achille.Internal.IO (AchilleIO)


-- | Recipe for loading a pandoc document
readPandoc :: MonadIO m
           => Recipe m FilePath Pandoc
readPandoc = readPandocWith def

-- | Recipe for loading a pandoc document using a given reader config
readPandocWith :: MonadIO m
               => ReaderOptions -> Recipe m FilePath Pandoc
readPandocWith ropts = nonCached \Context{..}  ->
    let ext = drop 1 $ takeExtension inputValue
        Just reader = lookup (pack ext) readers
    in case reader of
        ByteStringReader f -> liftIO $ 
            LazyByteString.readFile (inputDir </> currentDir </> inputValue)
                >>= runIOorExplode <$> f ropts
        TextReader f -> liftIO $
            Text.readFile (inputDir </> currentDir </> inputValue)
                >>= runIOorExplode <$> f ropts

-- | Recipe for loading a pandoc document and a frontmatter header.
readPandocMetadata :: (MonadIO m, MonadFail m, FromJSON a)
                   => Recipe m FilePath (a, Pandoc)
readPandocMetadata = readPandocMetadataWith def

-- | Recipe for loading a pandoc document using a given reader config
readPandocMetadataWith :: (MonadIO m, MonadFail m, FromJSON a)
                       => ReaderOptions -> Recipe m FilePath (a, Pandoc)
readPandocMetadataWith ropts = nonCached \Context{..} -> do
    let ext         = drop 1 $ takeExtension inputValue
        Just reader = lookup (pack ext) readers
    contents <- liftIO $ ByteString.readFile (inputDir </> currentDir </> inputValue)
    (meta, remaining) <-
            case parseYamlFrontmatter contents of
                Done i a -> pure (a, i)
                _        -> fail $ "error while loading meta of " <> inputValue
    (meta,) <$> case reader of
        ByteStringReader f -> liftIO $
            runIOorExplode $ f ropts (LazyByteString.fromStrict remaining)
        TextReader f -> liftIO $
            runIOorExplode $ f ropts (decodeUtf8 remaining)

-- | Recipe to convert a Pandoc document to HTML.
renderPandoc :: MonadIO m
             => Pandoc -> Recipe m a Text
renderPandoc = renderPandocWith def 

-- | Recipe to convert a Pandoc document to HTML using specified writer options.
renderPandocWith :: MonadIO m
                 => WriterOptions -> Pandoc -> Recipe m a Text
renderPandocWith wopts = liftIO . runIOorExplode <$> writeHtml5String wopts

-- | Recipe to load and convert a Pandoc document to HTML.
compilePandoc :: MonadIO m
              => Recipe m FilePath Text
compilePandoc = readPandoc >>= renderPandoc

-- | Recipe to load and convert a Pandoc document to HTML.
compilePandocWith :: MonadIO m
                  => ReaderOptions -> WriterOptions -> Recipe m FilePath Text
compilePandocWith ropts wopts =
    readPandocWith ropts >>= renderPandocWith wopts
