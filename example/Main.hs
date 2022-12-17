module Main where

import Prelude (IO, undefined, (<$>), (<*>), ($))
import Control.Monad (forM_)
import Control.Category ((.))
import Data.Monoid   ((<>))
import Data.Text     (Text, pack)
import Data.Aeson    (FromJSON)
import Data.Binary   (Binary)
import GHC.Generics  (Generic)
import Lucid
import System.FilePath

import Achille as A
import Achille.Pandoc
import Achille.Writable (Writable)
import Achille.Writable qualified as Writable
import qualified Prelude


main :: IO ()
main = achille rules

rules :: Task IO ()
rules = recipe \_ -> A.do
  posts <-
    match "posts/*.md" \src -> A.do
      meta ::: txt <- processPandocMeta src
      meta ::: write (src -<.> "html") (renderPost <$> meta <*> txt)

  write "index.html" (renderIndex <$> sortOn (date . Prelude.fst) posts)

  unit


instance Writable IO (Html ()) where
  write = undefined

data Meta = Meta
  { title :: Text
  , date  :: Text
  } deriving (Generic, FromJSON, Binary)


outer :: Html () -> Html ()
outer body = doctypehtml_ do
  header_ $ h1_ "My beautiful site"
  main_ body
  footer_ $ p_ "2022 All rights reserved"


renderPost :: Meta -> Text -> Html ()
renderPost Meta{..} content = outer $ do
  article_ do
    h1_      $ toHtml title
    p_       $ "Published on " <> toHtml date
    section_ $ toHtmlRaw content


renderIndex :: [(Meta, FilePath)] -> Html ()
renderIndex posts = outer do
  h2_ "Articles"
  ul_ $ forM_ posts \(Meta{..}, url) -> li_ do
    a_ [href_ (pack url)] $ toHtml title
