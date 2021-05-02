{-# LANGUAGE RecordWildCards #-}

-- | Core recipes including reading files, saving them.
module Achille.Task
    ( Task
    , liftIO
    , getCurrentDir
    , readText
    , readBS
    , readLBS
    , saveFileAs
    , copyFile
    , copyFileAs
    , copy
    , write
    , debug
    , callCommand
    , readCommand
    , callCommandWith
    , toTimestamped
    , toAbsolute
    , getOutputDir
    ) where

import Control.Monad.IO.Class  (liftIO)
import Data.Binary             (Binary, encodeFile)
import Data.Functor            (void)
import Data.Text               (Text, pack)
import Data.Text.Encoding      (decodeUtf8)
import Data.ByteString         (ByteString)
import System.FilePath         ((</>))

import qualified Data.ByteString.Lazy as LBS

import           Achille.Config
import           Achille.Writable (Writable)
import           Achille.Timestamped
import qualified Achille.Writable as Writable
import Achille.Internal    as Internal
import Achille.Internal.IO (AchilleIO)
import qualified Achille.Internal.IO as AchilleIO


data Color = Red | Blue

color :: Color -> String -> String
color c x = "\x1b[" <> start <> x <> "\x1b[0m"
    where start = case c of
                      Red  -> "31m"
                      Blue -> "34m"

-------------------------------------
-- Recipe building blocks (uncached)
-------------------------------------

-- | Recipe for retrieving the current directory
getCurrentDir :: Applicative m
              => Task m FilePath
getCurrentDir = nonCached (pure . Internal.currentDir)

-- | Recipe retrieving the contents of the input file as text
readText :: AchilleIO m
         => FilePath -> Task m Text
readText p = nonCached \Context{..} ->
    decodeUtf8 <$> AchilleIO.readFile (inputDir </> currentDir </> p)

-- | Recipe retrieving the contents of the input file as a bytestring.
readBS :: AchilleIO m => FilePath -> Task m ByteString
readBS p = nonCached \Context{..} ->
    AchilleIO.readFile (inputDir </> currentDir </> p)

-- | Recipe retrieving the contents of the input file as a lazy bytestring.
readLBS :: AchilleIO m => FilePath -> Task m LBS.ByteString
readLBS p = nonCached \Context{..} ->
    AchilleIO.readFileLazy (inputDir </> currentDir </> p)

-- | Recipe for saving a value to a file, using the path modifier applied to the input filepath.
--   Returns the path of the output file.
saveFileAs :: (AchilleIO m, Writable m a)
           => (FilePath -> FilePath)
           -> FilePath
           -> a
           -> Task m FilePath
saveFileAs mod p x = write (mod p) x

-- | Recipe for copying an input file to the same location in the output dir.
copyFile :: AchilleIO m
         => FilePath -> Task m FilePath
copyFile p = copy p p

-- | Recipe for copying an input file to the output dir, using the path modifier.
copyFileAs :: AchilleIO m
           => (FilePath -> FilePath)
           -> FilePath
           -> Task m FilePath
copyFileAs mod p = copy p (mod p)

-- | Recipe for copying a file to a given destination.
--   Returns the output filepath.
copy :: AchilleIO m
     => FilePath -> FilePath -> Task m FilePath
copy from to = nonCached \Context{..} -> do
    AchilleIO.copyFile (inputDir  </> currentDir </> from)
                       (outputDir </> currentDir </> to)
    AchilleIO.log (color Blue $ (currentDir </> from) <> " → " <> (currentDir </> to))
    pure to

-- | Recipe for writing to a an output file.
--   Returns the output filepath.
write :: (AchilleIO m, Writable m b)
      => FilePath -> b -> Task m FilePath
write p x = nonCached \Context{..} -> do
    Writable.write (outputDir  </> currentDir </> p) x
    AchilleIO.log (color Blue $ "writing " <> (currentDir </> p))
    pure p


------------------------------
-- Lifted IO
------------------------------

-- | Recipe for printing a value to the console.
debug :: AchilleIO m
      => Show b => b -> Task m ()
debug = nonCached . const . AchilleIO.log . show

-- | Recipe for running a shell command in a new process.
callCommand :: AchilleIO m
            => String -> Task m ()
callCommand = nonCached . const . AchilleIO.callCommand

-- | Recipe for running a shell command in a new process.
readCommand :: AchilleIO m
            => String -> [String] -> Task m String
readCommand cmd args = nonCached $ const $ AchilleIO.readCommand cmd args

-- | Recipe for running a shell command in a new process.
--   The command is defined with an helper function which depends on an input filepath
--   and the same filepath with a modifier applied.
--
--   Examples:
--
--   > cp :: Recipe IO FilePath FilePath
--   > cp = runCommandWith id (\a b -> "cp " <> a <> " " <> b)
callCommandWith :: AchilleIO m
                => (FilePath -> FilePath -> String)
                -> (FilePath -> FilePath)
                -> FilePath -> Task m FilePath
callCommandWith cmd mod p = nonCached \Context{..} ->
    let p' = mod p
    in AchilleIO.callCommand (cmd (inputDir </> currentDir </> p)
                                  (outputDir </> currentDir </> p'))
       >> pure p'

-- | Recipe that will retrieve the datetime contained in a filepath.
toTimestamped :: Monad m => FilePath -> Task m (Timestamped FilePath)
toTimestamped = pure . timestamped

toAbsolute :: Monad m => FilePath -> Task m FilePath
toAbsolute p = nonCached \Context{..} ->
    pure (inputDir </> currentDir </> p)

getOutputDir :: Monad m => Task m FilePath
getOutputDir = nonCached \Context{..} ->
    pure outputDir
