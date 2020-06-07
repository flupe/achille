{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}

module Templates where

import Data.String (fromString)
import Control.Monad (forM_)
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

toLink :: FilePath -> Html -> Html
toLink url = H.a ! A.href (fromString $ "/" <> url)

renderIndex :: [FilePath] -> Html -> Html
renderIndex posts content = 
    outer do
        content
        H.ul $ forM_ posts (H.li . (`toLink` "post"))

renderPost :: FilePath -> Html -> Html
renderPost source content =
    outer do
        toLink source "View source"
        content

outer :: Html -> Html
outer content = H.docTypeHtml do
    H.head do
        H.meta ! charset "utf-8"
        H.link ! A.rel "stylesheet" ! A.href "/assets/theme.css"
        H.title "sbbls"

    H.body do
        H.header ! A.id "hd" $ do
            H.section $ H.nav $ H.a ! A.href "/quid.html" $ "Quid"

        H.main content

        H.footer ! A.id "ft" $ do
            "flupe 2020 · "
            H.a ! A.href "https://creativecommons.org/licenses/by-nc/2.0/" $ "CC BY-NC 2.0"
            " · "
            H.a ! A.href "https://instagram.com/ba.bou.m/" $ "instagram"
