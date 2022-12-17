module Main where

import Prelude (IO, undefined)
import Data.Text   (Text)
import Data.Aeson  (FromJSON)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Lucid
import System.FilePath

import Achille as A
import Achille.Pandoc

data Meta = Meta
  { title :: Text
  , date  :: Text
  } deriving (Generic, FromJSON, Binary)

main :: IO ()
main = achille rules

rules :: Task IO ()
rules = recipe \_ -> A.do
  posts <-
    match "posts/*.md" \src -> A.do
      copyFile src
      (meta, txt) <- processPandocMeta src
      writeFile (src -<.> "html") (renderPost meta txt)
      meta

  writeFile "index.html" renderIndex posts

renderPost :: () -> Text -> Html ()
renderPost _ _ = undefined

renderIndex :: Text -> Html ()
renderIndex _ = undefined
