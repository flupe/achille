{-# LANGUAGE StandaloneDeriving, RecordWildCards #-}
module Achille.Stache.Recipe
  ( loadTemplate
  , loadTemplates
  , applyTemplate
  ) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Monad (forM, unless)
import Control.Arrow (arr)
import Data.Aeson (ToJSON(toJSON))
import Data.Binary (Binary)
import Data.Functor (($>))
import Data.Maybe (maybe, fromMaybe)
import Data.Map.Strict (Map)
import Data.Text (pack)
import Data.Text.Lazy (Text)
import Text.Megaparsec.Pos (Pos)
import Text.Mustache

import Data.Map.Strict qualified as Map

import Achille.Context (Context(..))
import Achille.Config
import Achille.DynDeps
import Achille.Diffable
import Achille.IO
import Achille.Path
import Achille.Recipe
import Achille.Task.Prim

-- standalone instances
deriving instance Binary PName
deriving instance Binary Key
deriving instance Binary Pos
deriving instance Binary Node
deriving instance Binary Template

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b x = b >>= \b -> unless b x

-- TODO(flupe): recursively fetch (and cache) partials?
loadTemplate :: Recipe IO Path Template
loadTemplate = recipe "loadTemplate" \vsrc -> do
  Context{..} <- getContext
  let Config{..} = siteConfig
  let path = contentDir </> theVal vsrc
  unlessM (doesFileExist path) $ fail ("Could not find template file " <> show path)
  mtime <- getModificationTime path
  setDeps (dependsOnFile path)
  fromCache >>= \case
    Just (t :: Template) | mtime <= lastTime, not (hasChanged vsrc) ->
      pure (value False t)
    mc -> do
      t <- lift $ compileMustacheFile (toFilePath path) -- TODO(flupe): handle parser exception gracefully
      toCache t
      pure (value (Just t /= mc) t)

tToNode :: Template -> [Node]
tToNode t = templateCache t Map.! templateActual t

-- TODO(flupe): transitive closure of template dependencies

-- | Load all templates from the input directory.
loadTemplates :: Recipe IO Path (Map PName Template)
loadTemplates = recipe "loadTemplates" \vdir -> do
  Context{..} <- getContext
  let Config{..} = siteConfig
  let dir = contentDir </> theVal vdir
  stored :: Map PName Template <- fromMaybe Map.empty <$> fromCache
  unlessM (doesDirExist dir) $ fail ("Could not find directory " <> show dir)
  files <- fmap (dir </>) . filter ((== ".mustache") . takeExtension) <$> listDir dir
  templates :: [Value Template] <- forM files \src -> do
    mtime <- getModificationTime src
    case stored Map.!? PName (pack $ takeBaseName src)  of
      Just (t :: Template) | mtime <= lastTime, not (hasChanged vdir) -> pure (value False t)
      mc -> do
        t <- lift $ compileMustacheFile (toFilePath src) -- TODO(flupe): handle parser exception gracefully
        pure (value (Just t /= mc) t)
  let
    tps :: Map PName (Value Template) =
      Map.fromList (zip (templateActual . theVal <$> templates) templates)
    tps' = tToNode . theVal <$> tps
    tps'' = (\(Value t c i) -> Value t { templateCache = tps' } c i) <$> tps
  setDeps (dependsOnFiles files)
  toCache (theVal <$> tps)
  pure (joinValue tps'')

applyTemplate :: (Monad m, ToJSON a) => Recipe m (Template, a) Text
applyTemplate = arr \(t, x) -> renderMustache t (toJSON x)
