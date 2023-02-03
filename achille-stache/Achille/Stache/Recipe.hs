{-# LANGUAGE StandaloneDeriving, RecordWildCards, ViewPatterns #-}

module Achille.Stache.Recipe
  ( loadTemplate
  , loadTemplates
  , applyTemplate
  ) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Monad (forM, unless, forM_)
import Control.Arrow (arr)
import Data.Bifunctor (first)
import Data.Aeson (ToJSON(toJSON))
import Data.Binary (Binary)
import Data.Functor (($>))
import Data.Maybe (maybe, fromMaybe)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (pack)
import Data.Text.Lazy (Text)
import Text.Megaparsec.Pos (Pos)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Mustache
import Debug.Trace (traceShowId)

import Data.Map.Strict    qualified as Map
import Data.Set           qualified as Set
import Data.Text.Encoding qualified as Text (decodeUtf8)

import Achille.Context (Context(..))
import Achille.Config
import Achille.DynDeps
import Achille.Diffable
import Achille.IO as AIO
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

nodePartials :: Node -> Set PName
nodePartials n =
  case n of
    Section _ ns -> foldMap nodePartials ns
    InvertedSection _ ns -> foldMap nodePartials ns
    Partial p _ -> Set.singleton p
    _ -> mempty

-- | Load all templates from the input directory.
loadTemplates :: Recipe IO Path (Map PName Template)
loadTemplates = recipe "loadTemplates" \vdir -> do
  Context{..} <- getContext
  let Config{..} = siteConfig
  let dir = contentDir </> theVal vdir

  unlessM (doesDirExist dir) $
    fail ("Could not find directory " <> show dir)

  -- In the cache, we store each template and its dependencies
  stored :: Map PName ([Node], Set PName) <- fromMaybe Map.empty <$> fromCache

  files <- fmap (dir </>) . filter ((== ".mustache") . takeExtension) <$> listDir dir

  templates :: Map PName (Value [Node], Set PName) <-
    Map.fromList <$> forM files \src -> do
      mtime <- getModificationTime src
      -- TODO(flupe): check pname policy
      let pname = PName (pack $ takeBaseName src)
      case stored Map.!? pname of
        Just (nodes, deps) | mtime <= lastTime, not (hasChanged vdir) -> pure (pname, (value False nodes, deps))
        mt -> do
          txt <- Text.decodeUtf8 <$> AIO.readFile src
          case compileMustacheText pname txt of
            Left err -> fail $ errorBundlePretty err
            Right (Template _ ((Map.! pname) -> nodes)) ->
              let changed = maybe False ((==nodes) . fst) mt
              in pure (pname, (value changed nodes, foldMap nodePartials nodes))

  let graph :: Map PName (Set PName) =
        Map.fromList [ (v, go Set.empty [v]) | v <- Map.keys templates ]
          where go :: Set PName -> [PName] -> Set PName
                go seen [] = seen
                go seen (v:vs) | v `Set.member` seen = go seen vs
                go seen (v:vs) = go (Set.insert v seen) (Set.toList (snd $ templates Map.! v) ++ vs)

  let
    bundleTemplate :: PName -> Set PName -> Value Template
    bundleTemplate pname deps =
      let children = Map.fromSet (fst . (templates Map.!)) deps
          changed  = any hasChanged children
       in value changed (Template pname (theVal <$> children))

    result :: Map PName (Value Template) = Map.mapWithKey bundleTemplate graph

  setDeps (dependsOnFiles files)
  toCache (Map.map (first theVal) templates)
  pure (joinValue result)

applyTemplate :: (Monad m, ToJSON a) => Recipe m (Template, a) Text
applyTemplate = arr \(t, x) -> renderMustache t (toJSON x)
