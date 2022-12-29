module Main where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Achille as A
import Achille.Pandoc
import Achille.Stache

data PostMeta = PostMeta
  { title :: Text
  } deriving (Generic, FromJSON, ToJSON)

data Post = Post
  { meta    :: PostMeta
  , content :: Text
  } deriving (Generic, ToJSON)

main :: IO ()
main = achille A.do
  tPost <- loadTemplate "templates/post.html"

  match_ "posts/*.md" \src -> A.do
    meta :*: doc <- processPandocMeta src
    write (src -<.> "html") (applyTemplate tPost (Post <$> meta <*> doc))

