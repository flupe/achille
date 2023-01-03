{-# LANGUAGE RecordWildCards #-}
-- | Some utilies to process YAML files with achille
module Achille.Yaml where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad (unless)
import Data.Binary (Binary)
import Data.Aeson (FromJSON)
import Data.Yaml (decodeEither', prettyPrintParseException)
import System.FilePath

import Achille.IO
import Achille.Cache
import Achille.Diffable
import Achille.Recipe (Context(..), recipe, readByteString)
import Achille.Task hiding (fail)


-- | Read a JSON-decodable value from a YAML file. The value will be cached
--   until the underlying file changes.
readYaml
  :: forall m a. (AchilleIO m, MonadFail m, FromJSON a, Eq a, Binary a)
  => Task m FilePath -> Task m a
readYaml = apply $ (id &&& readByteString)
  >>> recipe "Achille.Yaml.readYaml" \Context{..} cache v -> do
  let (vsrc, vbs) = splitValue v
  let stored :: Maybe a = fromCache cache
      path = inputRoot </> currentDir </> theVal vsrc
  mtime <- getModificationTime path
  case stored of
    Just x | mtime <= lastTime -> pure (value False x, cache)
    _ -> do
      case decodeEither' (theVal vbs) of
        Left err -> fail $ prettyPrintParseException err
        Right x -> case stored of
          Just y | x == y -> pure (value False x, cache)
          _               -> pure (value True x, toCache x)