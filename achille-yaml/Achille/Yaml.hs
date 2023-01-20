{-# LANGUAGE RecordWildCards #-}
-- | Some utilies to process YAML files with achille
module Achille.Yaml where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad (unless)
import Data.Aeson (FromJSON)
import Data.Binary (Binary)
import Data.Functor (($>))
import Data.Yaml (decodeEither', prettyPrintParseException)

import Achille.IO
import Achille.Config (Config(..))
import Achille.Context (Context(..))
import Achille.Diffable
import Achille.Path
import Achille.Recipe (recipe, readByteString)
import Achille.Task.Prim
import Achille.Task hiding (fail)


-- | Read a JSON-decodable value from a YAML file. The value will be cached
--   until the underlying file changes.
readYaml
  :: forall m a. (AchilleIO m, MonadFail m, FromJSON a, Eq a, Binary a)
  => Task m Path -> Task m a
readYaml = apply $ (id &&& readByteString)
  >>> recipe "Achille.Yaml.readYaml" \v -> do
  Context{..} <- getContext
  let Config{..} = siteConfig
  let (vsrc, vbs) = splitValue v
  stored :: Maybe a <- fromCache
  let path = contentDir </> currentDir </> theVal vsrc
  mtime <- getModificationTime path
  case stored of
    Just x | mtime <= lastTime -> pure (value False x)
    _ -> do
      case decodeEither' (theVal vbs) of
        Left err -> fail $ prettyPrintParseException err
        Right x -> case stored of
          Just y | x == y -> pure (value False x)
          _               -> toCache x $> value True x
