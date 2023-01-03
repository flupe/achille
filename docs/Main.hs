module Main where

import Prelude hiding (reverse, take, map)
import Data.Function ((&))
import Data.Binary (Binary(..))
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Aeson
import Text.Mustache.Type (PName (..))
import GHC.Generics (Generic)

import Achille as A
import Achille.Pandoc
import Achille.Stache

deriving newtype instance FromJSON PName
deriving newtype instance ToJSON PName

data PageMeta = Meta
  { title       :: Text
  , description :: Maybe Text
  , template    :: Maybe PName
  } deriving (Eq, Generic, Binary, FromJSON, ToJSON)

data Page = Page
  { meta    :: PageMeta
  , content :: Text
  } deriving (Generic, ToJSON)

data MenuItem = MenuItem
  { title   :: Text
  , source  :: FilePath
  , current :: Maybe Bool
  } deriving (Generic, FromJSON, ToJSON)

main :: IO ()
main = achille A.do
  match_ "assets/*" copy

  templates <- loadTemplates "templates"

  match_ "**/*.md" \src -> A.do
    meta :*: content <- processPandocMeta src
    template <- templates ! (fromMaybe "page" . template <$> meta)
    (Page <$> meta <*> content)
      & applyTemplate template
      & write (src -<.> "html")
