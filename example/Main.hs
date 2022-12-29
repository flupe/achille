module Main where

import Data.Function ((&))
import Data.Binary (Binary(..))
import Data.Text (Text)
import Data.Time
import Data.Aeson
import GHC.Generics (Generic)

import Achille as A
import Achille.Pandoc
import Achille.Stache

data PostMeta = PostMeta
  { title :: Text
  , date  :: UTCTime
  , tags  :: Maybe [Text]
  } deriving (Generic, FromJSON, Binary)

data Post     = Post { meta :: PostMeta, content :: Text }     deriving (Generic, ToJSON)
data PostItem = PostItem { url :: FilePath, meta :: PostMeta } deriving (Generic, Binary, ToJSON)

instance ToJSON PostMeta where
  toJSON (PostMeta t d tags) = object 
    [ ("title", toJSON t)
    , ("date", toJSON $ formatTime defaultTimeLocale "%a, %e %b" d)
    , ("tags", toJSON tags)
    ]

main :: IO ()
main = achille A.do
  tpost  <- loadTemplate "templates/post.html"
  tindex <- loadTemplate "templates/index.html"

  posts <- match "posts/*.md" \src -> A.do
    meta :*: content <- processPandocMeta src
    url <- applyTemplate tpost (Post <$> meta <*> content) 
            & write (src -<.> "html")
    PostItem <$> url <*> meta

  applyTemplate tindex (("posts",) <$> posts :: Task IO (Text, [PostItem]))
    & write "index.html"
