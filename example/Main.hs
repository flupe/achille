module Main where

import Data.Aeson.Types (object)
import Data.Time (defaultTimeLocale, formatTime)

import Achille as A
import Achille.Prelude
import Achille.Pandoc
import Achille.Stache

data PostMeta = PostMeta
  { title :: Text
  , date  :: UTCTime
  , tags  :: Maybe [Text]
  } deriving (Eq, Generic, FromJSON, Binary)

data Post = Post
  { meta    :: PostMeta
  , content :: Text
  } deriving (Generic, ToJSON)

newtype Index = Index
  { posts :: [PostItem] }
  deriving newtype ToJSON

data PostItem = PostItem
  { url  :: Text
  , meta :: PostMeta
  } deriving (Eq, Generic, Binary, ToJSON)

instance ToJSON PostMeta where
  toJSON (PostMeta t d tags) = object
    [ ("title", toJSON t)
    , ("date", toJSON $ formatTime defaultTimeLocale "%a, %e %b" d)
    , ("tags", toJSON tags)
    ]

main :: IO ()
main = achille A.do
  match "assets/*" (void . copy)
  templates <- loadTemplates "templates"

  posts <- match "posts/*.md" \src -> A.do
    meta :*: content <- processPandocMeta src
    url <- applyTemplate (templates ! "post") (Post <$> meta <*> content) 
             & write (src -<.> "html")
    PostItem <$> url <*> meta

  mostRecent <- sortOn (.meta.date) posts & reverse & take 10

  applyTemplate (templates ! "index") (Index <$> mostRecent)
    & write "index.html"
