module Main where

import Prelude hiding (reverse, take)
import Data.Function ((&))
import Data.Binary (Binary(..))
import Data.Text (Text)
import Data.Time
import Data.Aeson
import GHC.Generics (Generic)

import Achille as A
import Achille.Pandoc
import Achille.Stache

data PageMeta = Meta
  { title     :: Text
  } deriving (Eq, Generic, Binary, FromJSON, ToJSON)

data Page = Page
  { meta    :: PageMeta
  , content :: Text
  } deriving (Eq, Generic, Binary, ToJSON)

data MenuItem = MenuItem
  {
  } deriving (Generic, ToJSON)

main :: IO ()
main = achille A.do
  match_ "assets/*" copy

  templates  <- loadTemplates "templates"

  match_ "**/*.md" \src ->
    uncurry Page <$> processPandocMeta src
      & applyTemplate (templates ! "page")
      & write (src -<.> "html")
