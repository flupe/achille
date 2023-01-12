module Achille.Pandoc.Recipe where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Frontmatter (IResult(..))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)

import Text.Pandoc.Error (renderError)
import Text.Pandoc.Class (PandocPure, PandocMonad, runPure)
import Text.Pandoc.Readers hiding (getReader)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions, WriterOptions, def)
import Text.Pandoc.Writers (writeHtml5String)

import Data.Yaml        qualified as Yaml
import Data.Frontmatter qualified as Frontmatter

import Achille.Diffable
import Achille.IO
import Achille.Path
import Achille.Recipe

-- TODO(flupe): caching/hashing of pandoc options

-- TODO(flupe): add more readers and common extensions
--              and probably fallback to plaintext?
getReader :: MonadFail m => String -> m (ReaderOptions -> Text -> PandocPure Pandoc)
getReader ".md"       = pure readMarkdown
getReader ".markdown" = pure readMarkdown
getReader ".rst"      = pure readRST
getReader ".org"      = pure readOrg
getReader ext = fail $ "Could not find Pandoc reader for extension " <> ext

-- TODO(flupe): make this simpler by providing (Value a -> m (Value b)) -> Recipe m a b in achille

-- | Read a pandoc document from path, using provided reader options.
readPandocWith
  :: (MonadFail m, Applicative m, AchilleIO m)
  => ReaderOptions -> Recipe m Path Pandoc
readPandocWith ropts = id &&& readText >>>
  recipe "readPandoc" \ctx cache v -> do
    let (vsrc, vtxt) = splitValue v
    let ext = takeExtension $ theVal vsrc
    reader <- getReader ext
    case runPure (reader ropts $ theVal vtxt) of
      Left err  -> fail $ unpack $ renderError err
      Right doc -> pure (value (hasChanged vtxt) doc, cache)

-- | Read a pandoc document and its frontmatter metadata from path, using provided reader options.
readPandocMetaWith
  :: (MonadFail m, AchilleIO m, FromJSON a)
  => ReaderOptions -> Recipe m Path (a, Pandoc)
readPandocMetaWith ropts = id &&& readByteString >>>
  recipe "readPandocMeta" \ctx cache v -> do
    let (vsrc, vbs) = splitValue v
        ext = takeExtension $ theVal vsrc
    reader <- getReader ext
    case Frontmatter.parseYamlFrontmatter $ theVal vbs of
      Done rest meta ->
        case runPure (reader ropts $ decodeUtf8 rest) of
          Left err -> fail $ show (theVal vsrc) <> ": " <> unpack (renderError err)
          Right doc -> pure (value (hasChanged v) (meta, doc), cache)
      -- TODO(flupe): better error-reporting
      -- TODO(flupe): cache front-matter?
      _ -> fail $ show (theVal vsrc) <> ": Could not parse YAML frontmatter"

-- | Convert pandoc document to text, using provided writer options.
renderPandocWith :: MonadFail m => WriterOptions -> Recipe m Pandoc Text
renderPandocWith wopts = recipe "renderPandoc" \ctx cache vdoc -> do
  case runPure (writeHtml5String wopts $ theVal vdoc) of
    Left err   -> fail $ unpack $ renderError $ err
    Right html -> pure (value (hasChanged vdoc) html, cache)
