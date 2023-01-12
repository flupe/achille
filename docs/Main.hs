module Main where

import Prelude hiding (reverse, take, fail)
import Data.Function      ((&))
import Data.Binary        (Binary(..))
import Data.Text          (Text)
import Data.Maybe         (fromMaybe)
import Data.Aeson         (FromJSON, ToJSON)
import Text.Mustache.Type (PName (..))
import GHC.Generics       (Generic)

import Achille as A
import Achille.Pandoc
import Achille.Path (Path)
import Achille.Stache
import Achille.Yaml

-- TODO(flupe): move this to achille-stache?
deriving newtype instance FromJSON PName
deriving newtype instance ToJSON PName
deriving anyclass instance FromJSON Path
deriving anyclass instance ToJSON Path

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
  , source  :: Maybe Path
  , url     :: Text
  , active  :: Maybe Bool
  } deriving (Eq, Generic, Binary, FromJSON, ToJSON)

-- The only reason we have to do this here is mustache being logic-less
setCurrent :: Path -> [MenuItem] -> [MenuItem]
setCurrent src = fmap set
  where set :: MenuItem -> MenuItem
        set item | item.source == Just src = item { active = Just True }
        set item = item

main :: IO ()
main = achille A.do
  match "assets/*" (void . copy)

  templates <- loadTemplates "templates"
  items :: Task IO [MenuItem] <- readYaml "menu.yml"

  match "**/*.md" \src -> A.do
    meta :*: content <- processPandocMeta src
    menu <- setCurrent <$> src <*> items
    template <- templates ! (fromMaybe "page" . template <$> meta)
    (Page <$> meta <*> menu <*> content)
      & applyTemplate template
      & write (src -<.> "html")
      & void
