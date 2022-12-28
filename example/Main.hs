module Main where

import Prelude
import Control.Monad (forM_)
import Data.Text     (Text, pack)
import Data.Aeson    (FromJSON, ToJSON)
import GHC.Generics  (Generic)

import Achille as A
import Achille.Pandoc
import Achille.Stache
import Achille.Writable qualified as Writable

-- things to add to Achille.Syntax

sortOn :: (Achille task, Ord b) => (a -> b) -> task m [a] -> task m [a]
sortOn = undefined

-- post meta information
-- data Meta = Meta
--   { title :: Text
--   , date  :: Text
--   } deriving (Generic, FromJSON, Binary)

data Post = Post
  { content :: Text
  } deriving (Generic, ToJSON)


-- main example
main :: IO ()
main = achille A.do
  template <- loadTemplate "templates/post.html"

  match_ "posts/*.md" \src -> A.do
    post <- Post <$> processPandoc src
    write (src -<.> "html") (applyTemplate template post)
