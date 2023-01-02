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

data PostMeta = PostMeta
  { title :: Text
  , date  :: UTCTime
  , tags  :: Maybe [Text]
  } deriving (Eq, Generic, FromJSON, Binary)

data Post = Post
  { meta :: PostMeta, content :: Text } 
  deriving (Generic, ToJSON)

data Index = Index
  { posts :: [PostItem] }
  deriving (Generic, ToJSON)

data PostItem = PostItem
  { url :: FilePath, meta :: PostMeta }
  deriving (Eq, Generic, Binary, ToJSON)

instance ToJSON PostMeta where
  toJSON (PostMeta t d tags) = object 
    [ ("title", toJSON t)
    , ("date", toJSON $ formatTime defaultTimeLocale "%a, %e %b" d)
    , ("tags", toJSON tags)
    ]

main :: IO ()
main = achille A.do
  match_ "assets/*" copy
  templates <- loadTemplates "templates"

  posts <- match "posts/*.md" \src -> A.do
    meta :*: content <- processPandocMeta src
    url <- applyTemplate (templates ! "post") (Post <$> meta <*> content) 
             & write (src -<.> "html")
    PostItem <$> url <*> meta

  mostRecent <- sortOn (.meta.date) posts & reverse & take 10

  applyTemplate (templates ! "index") (Index <$> mostRecent)
    & write "index.html"
