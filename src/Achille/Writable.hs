{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Achille.Writable where

import Data.Text                as Text
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import Data.ByteString      as BS
import Data.ByteString.Lazy as LBS

import qualified Data.ByteString.Lazy as ByteString

import Achille.Internal.IO as AchilleIO


-- | Class for things that can be saved.
class Writable m a where
    write :: FilePath -> a -> m ()

instance AchilleIO m => Writable m [Char] where
    write to = write to . Text.pack

instance AchilleIO m => Writable m Text where
    write to = AchilleIO.writeFile to . encodeUtf8

instance AchilleIO m => Writable m LT.Text where
    write to = AchilleIO.writeFileLazy to . LT.encodeUtf8

instance AchilleIO m => Writable m BS.ByteString where
    write = AchilleIO.writeFile

instance AchilleIO m => Writable m LBS.ByteString where
    write = AchilleIO.writeFileLazy
