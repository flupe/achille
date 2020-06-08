{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}

module Templates where

import Data.String (fromString)
import Control.Monad (forM_)

import Text.Blaze.Internal         as I
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

import Timestamped
import Thumbnail

loading :: AttributeValue -> Attribute
loading = I.customAttribute "loading"

toLink :: FilePath -> Html -> Html
toLink url = H.a ! A.href (fromString $ "/" <> url)

renderIndex :: [FilePath] -> Html -> Html
renderIndex posts content = 
    outer do
        content
        H.h2 "Latest notes"
        H.ul $ forM_ posts (H.li . (`toLink` "post"))

renderPost :: FilePath -> Html -> Html
renderPost source content =
    outer do
        toLink source "View source"
        content

renderVisual :: Html -> [Timestamped Thumbnail] -> Html
renderVisual txt imgs =
    outer do
        txt
        H.hr
        H.section $ forM_ imgs \ (Timestamped _ p) ->
            H.figure $ H.img ! A.src    (fromString $ thumbPath p)
                             ! A.width  (fromString . show $ thumbWidth p)
                             ! A.height (fromString . show $ thumbHeight p)
                             ! loading "lazy"

logo :: Html
logo = preEscapedString "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" height=\"19px\" width=\"29px\"><path d=\"M 2,2 A 5,5 0 0 1 7,7 L 7, 12 A 5, 5 0 0 1 2,17 M 7,7 A 5,5 0 0 1 12,2 L 22,2 A 5,5 0 0 1 27,7 L 27,12 A 5, 5 0 0 1 22,17 L 12,17\" style=\"stroke-width: 2; stroke-linecap: butt; stroke-linejoin: bevel; stroke: #fff\" fill=\"none\"/></svg>"

outer :: Html -> Html
outer content = H.docTypeHtml do
    H.head do
        H.meta ! charset "utf-8"
        H.link ! A.rel "stylesheet" ! A.href "/assets/theme.css"
        H.title "sbbls"

    H.body do
        H.header ! A.id "hd" $ H.section do
            H.a ! A.href "/" $ logo
            H.section $ H.nav do
                H.a ! A.href "/quid.html"   $ "Quid"
                H.a ! A.href "/visual.html" $ "Visual"

        H.main content

        H.footer ! A.id "ft" $ do
            "flupe 2020 · "
            H.a ! A.href "https://creativecommons.org/licenses/by-nc/2.0/" $ "CC BY-NC 2.0"
            " · "
            H.a ! A.href "https://instagram.com/ba.bou.m/" $ "instagram"
