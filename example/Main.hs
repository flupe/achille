module Main where

import Prelude
import Control.Monad (forM_)
import Data.Text     (Text, pack)
import Data.Aeson    (FromJSON)
import Data.Binary   (Binary)
import GHC.Generics  (Generic)
import Lucid
import Lucid.Base qualified as Lucid

import Achille as A
import Achille.Pandoc
import Achille.Writable qualified as Writable

-- things to add to Achille.Syntax

sortOn :: (Achille task, Ord b) => (a -> b) -> task m [a] -> task m [a]
sortOn = undefined

-- post meta information
data Meta = Meta
  { title :: Text
  , date  :: Text
  } deriving (Generic, FromJSON, Binary)


-- main example
main :: IO ()
main = achille A.do
  -- posts <-
    match_ "posts/*.md" \src -> A.do
      txt <- processPandoc src
      write (src -<.> "html") (renderPost <$> txt)

  -- write undefined (renderIndex <$> sortOn (date . Prelude.fst) posts)

-- telling achille how to write Lucid HTML to file
instance Writable.Writable IO (Html ()) where
  write p = Writable.write p . Lucid.renderBS


outer :: Html () -> Html ()
outer body = doctypehtml_ do
  header_ $ h1_ "My beautiful site"
  main_ body
  footer_ $ p_ "2022 All rights reserved"


renderPost :: Text -> Html ()
renderPost content = outer $ do
  article_ do
    -- h1_      $ toHtml title
    -- p_       $ "Published on " <> toHtml date
    section_ $ toHtmlRaw content


renderIndex :: [(Meta, FilePath)] -> Html ()
renderIndex posts = outer do
  h2_ "Articles"
  ul_ $ forM_ posts \(Meta{..}, url) -> li_ do
    a_ [href_ (pack url)] $ toHtml title
