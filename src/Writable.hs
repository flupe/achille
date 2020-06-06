{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Writable where

import Data.Text  (Text)
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html (Html)
import Codec.Picture (PngSavable, Image, writePng)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Data.Text.IO         as Text
import qualified Data.ByteString.Lazy as ByteString


class Writable a where
    write :: FilePath -> a -> IO ()


instance Writable [Char] where
    write = writeFile

instance Writable Text where
    write = Text.writeFile

instance Writable Html where
    write p = ByteString.writeFile p . renderHtml

instance PngSavable pixel => Writable (Image pixel) where
    write = writePng
