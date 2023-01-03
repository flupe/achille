module Main where

import Prelude hiding (reverse, take, map, fail)
import Control.Monad (unless)
import Data.Function ((&))
import Data.Binary (Binary(..))
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Aeson
import System.FilePath ((</>))
import Text.Mustache.Type (PName (..))
import GHC.Generics (Generic)

import Achille as A
import Achille.Diffable
import Achille.Recipe (Context(..), recipe)
import Achille.Pandoc
import Achille.Cache
import Achille.Stache
import Achille.IO
import Prelude qualified
import Data.Aeson qualified as Aeson
import Data.Yaml (decodeFileThrow)

deriving newtype instance FromJSON PName
deriving newtype instance ToJSON PName

data PageMeta = Meta
  { title       :: Text
  , description :: Maybe Text
  , template    :: Maybe PName
  } deriving (Eq, Generic, Binary, FromJSON, ToJSON)

data Page = Page
  { meta    :: PageMeta
  , menu    :: [MenuItem]
  , content :: Text
  } deriving (Generic, ToJSON)

data MenuItem = MenuItem
  { title   :: Text
  , source  :: Maybe FilePath
  , url     :: Text
  , active  :: Maybe Bool
  } deriving (Eq, Generic, Binary, FromJSON, ToJSON)


-- TODO(flupe): move this to achille-yaml
readYaml :: forall a. (FromJSON a, Binary a, Eq a) => Task IO FilePath -> Task IO a
readYaml = apply $ recipe "readYaml" \Context{..} cache vp -> do
  let stored :: Maybe a = fromCache cache
      path = inputRoot </> currentDir </> theVal vp
  exists <- doesFileExist path
  unless exists $ Prelude.fail $ "Could not find file: " <> path
  mtime <- getModificationTime path
  case stored of
    Just x | mtime <= lastTime -> pure (value False x, cache)
    _ -> do
      x :: a <- decodeFileThrow path
      case stored of
        Just y | x == y -> pure (value False x, cache)
        _ -> pure (value True x, toCache x)


setCurrent :: FilePath -> [MenuItem] -> [MenuItem]
setCurrent src = Prelude.map set
  where set :: MenuItem -> MenuItem
        set item | item.source == Just src = item { active = Just True }
        set item = item

main :: IO ()
main = achille A.do
  match_ "assets/*" copy

  templates <- loadTemplates "templates"
  items :: Task IO [MenuItem] <- readYaml "menu.yml"

  match_ "**/*.md" \src -> A.do
    meta :*: content <- processPandocMeta src
    menu <- setCurrent <$> src <*> items
    template <- templates ! (fromMaybe "page" . template <$> meta)
    (Page <$> meta <*> menu <*> content)
      & applyTemplate template
      & write (src -<.> "html")
