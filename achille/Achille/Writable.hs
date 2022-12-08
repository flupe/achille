{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines an interface for things that can be written to disk.
module Achille.Writable where

import Data.Text as Text
import Data.Text.Encoding (encodeUtf8)

import Data.Text.Lazy          qualified as LT
import Data.Text.Lazy.Encoding qualified as LT (encodeUtf8)
import Data.ByteString         qualified as BS
import Data.ByteString.Lazy    qualified as LBS

import Achille.IO as AchilleIO


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
