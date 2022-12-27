module Main where

import Prelude
import Control.Monad (forM_)
import Control.Applicative
import Data.Functor (Functor)
import Data.Ord (Ord)
import Data.Monoid   ((<>))
import Data.Text     (Text, pack)
import Data.Aeson    (FromJSON)
import Data.Binary   (Binary)
import GHC.Generics  (Generic)
import Lucid
import System.FilePath (FilePath)

import Achille as A
import Achille.Pandoc
import Achille.Writable (Writable)
import Achille.Writable qualified as Writable
import qualified Prelude

-- things to add to Achille.Syntax
write :: Achille task => task m FilePath -> task m a -> task m FilePath
write = undefined

(-<.>) :: Achille task => task m FilePath -> FilePath -> task m FilePath
(-<.>) = undefined

sortOn :: (Achille task, Ord b) => (a -> b) -> task m [a] -> task m [a]
sortOn = undefined

instance (Achille task, Functor m) => Functor (task m) where
instance (Achille task, Applicative m) => Applicative (task m) where

-- main example
main :: IO ()
main = achille A.do
  posts <-
    match "posts/*.md" \src -> A.do
      meta :*: txt <- processPandocMeta src
      meta :*: write (src -<.> "html") (renderPost <$> meta <*> txt)

  write undefined (renderIndex <$> sortOn (date . Prelude.fst) posts)


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
