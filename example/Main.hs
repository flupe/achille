module Main where

import Data.Text (Text)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import Achille as A
import Achille.Pandoc
import Achille.Stache

data Post = Post
  { content :: Text
  } deriving (Generic, ToJSON)

main :: IO ()
main = achille A.do
  tPost  <- loadTemplate "templates/post.html"

  match_ "posts/*.md" \src -> A.do
    post <- Post <$> processPandoc src
    write (src -<.> "html") (applyTemplate tPost post)
