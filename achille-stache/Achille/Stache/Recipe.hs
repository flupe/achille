{-# LANGUAGE StandaloneDeriving, RecordWildCards #-}
module Achille.Stache.Recipe
  ( loadTemplate
  , loadTemplates
  , applyTemplate
  ) where

import Prelude hiding ((.), id)

import Data.Maybe (maybe, fromMaybe)
import Control.Category
import Control.Monad (forM)
import Control.Arrow (arr)
import Data.Binary (Binary)
import Data.Aeson (ToJSON(toJSON))
import Data.Text (pack)
import Data.Text.Lazy (Text)
import Data.Map.Strict (Map)
import System.FilePath ((</>), takeExtension, takeBaseName)
import Text.Mustache
import Text.Megaparsec.Pos (Pos)

import Data.Map.Strict qualified as Map

import Achille.Cache
import Achille.Diffable
import Achille.IO
import Achille.Recipe

-- standalone instances
deriving instance Binary PName
deriving instance Binary Key
deriving instance Binary Pos
deriving instance Binary Node
deriving instance Binary Template

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b x = b >>= \b -> if b then pure () else x

-- TODO(flupe): recursively fetch (and cache) partials?
loadTemplate :: Recipe IO FilePath Template
loadTemplate = recipe "loadTemplate" \Context{..} cache vsrc -> do
  let path = inputRoot </> theVal vsrc
  unlessM (doesFileExist path) $ fail ("Could not find template file " <> path)
  mtime <- getModificationTime path
  case fromCache cache of
    Just (t :: Template) | mtime <= lastTime, not (hasChanged vsrc) -> pure (value False t, cache)
    mc -> do
      t <- compileMustacheFile path -- TODO(flupe): handle parser exception gracefully
      let changed = maybe False (== t) mc
      pure (value changed t, toCache t)

tToNode :: Template -> [Node]
tToNode t = templateCache t Map.! templateActual t


-- | Load all templates from the input directory.
loadTemplates :: Recipe IO FilePath (Map PName Template)
loadTemplates = recipe "loadTemplates" \Context{..} cache vdir -> do
  let dir = inputRoot </> theVal vdir
  let stored :: Map PName Template = fromMaybe Map.empty (fromCache cache)
  unlessM (doesDirExist dir) $ fail ("Could not find directory " <> dir)
  files <- fmap (dir </>) . filter ((== ".mustache") . takeExtension) <$> listDir dir
  templates :: [Value Template] <- forM files \src -> do
    mtime <- getModificationTime src
    case stored Map.!? (PName (pack $ takeBaseName src))  of
      Just (t :: Template) | mtime <= lastTime, not (hasChanged vdir) -> pure (value False t)
      mc -> do
        t <- compileMustacheFile src -- TODO(flupe): handle parser exception gracefully
        let changed = maybe False (== t) mc
        pure (value changed t)
  let
    tps :: Map PName (Value Template) =
      Map.fromList (zip (templateActual . theVal <$> templates) templates)
    tps' = tToNode . theVal <$> tps
    tps'' = (\(Value t c i) -> Value t { templateCache = tps' } c i) <$> tps
  pure (joinValue tps'', toCache (theVal <$> tps))

applyTemplate :: (Applicative m, ToJSON a) => Recipe m (Template, a) Text
applyTemplate = arr \(t, x) -> renderMustache t (toJSON x)
