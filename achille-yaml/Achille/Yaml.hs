{-# LANGUAGE RecordWildCards, DerivingStrategies #-}
-- | Some utilies to process YAML files with achille
module Achille.Yaml
  ( readYaml
  , FromJSON
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Functor (($>))
import Data.Yaml (decodeEither', prettyPrintParseException)

import Achille.IO as AIO
import Achille.Config (Config(..))
import Achille.Context (Context(..))
import Achille.Diffable
import Achille.DynDeps
import Achille.Path
import Achille.Recipe (recipe, readByteString)
import Achille.Task.Prim
import Achille.Task hiding (fail)


-- standalone instances
instance FromJSON Path
instance ToJSON Path

-- | Read a JSON-decodable value from a YAML file. The value will be cached
--   until the underlying file changes.
readYaml
  :: forall m a. (AchilleIO m, MonadFail m, FromJSON a, Eq a, Binary a)
  => Task m Path -> Task m a
readYaml = apply $ recipe "Achille.Yaml.readYaml" \vsrc -> do
  Context{..} <- getContext
  let Config{..} = siteConfig
  stored :: Maybe a <- fromCache
  let path = contentDir </> currentDir </> theVal vsrc
  setDeps (dependsOnFile path)
  mtime <- getModificationTime path
  case stored of
    Just x | mtime <= lastTime, not (hasChanged vsrc) -> pure (value False x)
    _ -> do
      vbs <- AIO.readFile path
      case decodeEither' vbs of
        Left err -> fail $ prettyPrintParseException err
        Right x -> case stored of
          Just y | x == y -> pure (value False x)
          _               -> toCache x $> value True x
