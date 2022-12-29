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
  } deriving (Generic, FromJSON, Binary)

data Post = Post
  { meta    :: PostMeta
  , content :: Text
  } deriving (Generic, ToJSON)

instance ToJSON PostMeta where
  toJSON (PostMeta t d) = object 
    [ ("title", toJSON t)
    , ("date", toJSON $ formatTime defaultTimeLocale "%a, %e %b" d)
    ]

main :: IO ()
main = achille A.do
  tpost  <- loadTemplate "templates/post.html"
  tindex <- loadTemplate "templates/index.html"

  posts <- match "posts/*.md" \src -> A.do
    front :*: content <- processPandocMeta src
    applyTemplate tpost (Post <$> front <*> content) 
      & write (src -<.> "html")
      & (:*: front)

  applyTemplate tindex (object <$> undefined)
    & write "index.html"
