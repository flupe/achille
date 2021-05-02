{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Defines convenience recipes for reading and writing documents with pandoc.
module Achille.Task.Pandoc
    ( readPandoc
    , readPandocWith
    , readPandocMetadata
    , readPandocMetadataWith
    , readAbsPandocMetadataWith
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
import Text.Pandoc      hiding (nonCached, getReader)
import Data.Aeson.Types (FromJSON)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Yaml hiding (Parser)
import Data.ByteString as ByteString hiding (pack)
import Control.Applicative ((<|>))

import qualified Data.Text.IO          as Text
import qualified System.FilePath.Glob  as Glob
import qualified Data.ByteString.Char8 as ByteString (pack)
import qualified Data.ByteString.Lazy  as LazyByteString
import qualified System.FilePath       as Path
import qualified System.Process        as Process

import           Achille.Config
import           Achille.Internal hiding (currentDir)
import qualified Achille.Internal as Internal
import           Achille.Task
import           Achille.Writable as Writable
import           Achille.Internal.IO (AchilleIO)


getReader :: PandocMonad m => FilePath -> Maybe (Reader m)
getReader p = byExt (takeExtension p)
    where
        byExt ".md"       = Just $ TextReader readMarkdown
        byExt ".markdown" = Just $ TextReader readMarkdown
        byExt ".rst"      = Just $ TextReader readRST
        byExt ".org"      = Just $ TextReader readOrg
        byExt _           = Nothing

parseYaml :: FromJSON a => ByteString -> Either String (a, ByteString)
parseYaml bs = case parse parser bs of
  Done i a   -> Right (a , i)
  Fail _ _ e -> Left e
  where
    parser = do
      bs <- ByteString.pack <$> (separator *> manyTill anyChar separator <|> pure "")
      case decodeEither' bs of
        Left e  -> fail (show e)
        Right v -> pure v
    separator :: Parser ()
    separator = string "---" >> endOfLine


-- | Recipe for loading a pandoc document
readPandoc :: (MonadIO m, MonadFail m)
           => FilePath -> Task m Pandoc
readPandoc = readPandocWith def

-- | Recipe for loading a pandoc document using a given reader config
readPandocWith :: (MonadIO m, MonadFail m)
               => ReaderOptions
               -> FilePath
               -> Task m Pandoc
readPandocWith ropts p = nonCached \Context{..}  ->
    case getReader p of
        Just (ByteStringReader f) -> liftIO $ 
            LazyByteString.readFile (inputDir </> currentDir </> p)
                >>= runIOorExplode <$> f ropts
        Just (TextReader f) -> liftIO $
            Text.readFile (inputDir </> currentDir </> p)
                >>= runIOorExplode <$> f ropts
        Nothing -> fail $ "No pandoc reader found for " <> p

-- | Recipe for loading a pandoc document and a frontmatter header.
readPandocMetadata :: (MonadIO m, MonadFail m, FromJSON a)
                   => FilePath
                   -> Task m (a, Pandoc)
readPandocMetadata = readPandocMetadataWith def

-- | Recipe for loading a pandoc document using a given reader config
readPandocMetadataWith :: (MonadIO m, MonadFail m, FromJSON a)
                       => ReaderOptions
                       -> FilePath
                       -> Task m (a, Pandoc)
readPandocMetadataWith ropts p = nonCached \Context{..} -> do
    contents <- liftIO $ ByteString.readFile (inputDir </> currentDir </> p)
    (meta, remaining) <-
            case parseYaml contents of
                Right res -> pure res
                Left  err -> fail $ "error while loading meta of " <> p <> ": " <> show err
    (meta,) <$> case getReader p of
        Just (ByteStringReader f) -> liftIO $
            runIOorExplode $ f ropts (LazyByteString.fromStrict remaining)
        Just (TextReader f) -> liftIO $
            runIOorExplode $ f ropts (decodeUtf8 remaining)
        Nothing -> fail $ "No pandoc reader found for " <> p

-- | Recipe for loading a pandoc document using a given reader config
readAbsPandocMetadataWith :: (MonadIO m, MonadFail m, FromJSON a)
                          => ReaderOptions
                          -> FilePath
                          -> Task m (a, Pandoc)
readAbsPandocMetadataWith ropts p = nonCached \Context{..} -> do
    contents <- liftIO $ ByteString.readFile p
    (meta, remaining) <-
            case parseYaml contents of
                Right res -> pure res
                Left  err -> fail $ "error while loading meta of " <> p <> ": " <> show err
    (meta,) <$> case getReader p of
        Just (ByteStringReader f) -> liftIO $
            runIOorExplode $ f ropts (LazyByteString.fromStrict remaining)
        Just (TextReader f) -> liftIO $
            runIOorExplode $ f ropts (decodeUtf8 remaining)
        Nothing -> fail $ "No pandoc reader found for " <> p

-- | Recipe to convert a Pandoc document to HTML.
renderPandoc :: MonadIO m
             => Pandoc -> Task m Text
renderPandoc = renderPandocWith def 

-- | Recipe to convert a Pandoc document to HTML using specified writer options.
renderPandocWith :: MonadIO m
                 => WriterOptions -> Pandoc -> Task m Text
renderPandocWith wopts = liftIO . runIOorExplode <$> writeHtml5String wopts

-- | Recipe to load and convert a Pandoc document to HTML.
compilePandoc :: (MonadIO m, MonadFail m)
              => FilePath -> Task m Text
compilePandoc p = readPandoc p >>= renderPandoc

-- | Recipe to load and convert a Pandoc document to HTML.
compilePandocWith :: (MonadIO m, MonadFail m)
                  => ReaderOptions
                  -> WriterOptions
                  -> FilePath
                  -> Task m Text
compilePandocWith ropts wopts p =
    readPandocWith ropts p >>= renderPandocWith wopts
