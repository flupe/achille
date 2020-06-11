{-# LANGUAGE RecordWildCards #-}

module Achille.Recipe
    ( Recipe
    , liftIO
    , getInput
    , getCurrentDir
    , readText
    , saveFile
    , saveFileAs
    , copyFile
    , copyFileAs
    , copy
    , write
    , task
    , debug
    , callCommand
    , runCommandWith
    , toTimestamped
    ) where


import Prelude hiding (read)

import Control.Monad.IO.Class  (MonadIO, liftIO)
import Data.Binary             (Binary, encodeFile)
import Data.Functor            (void)
import Data.Text               (Text, pack)
import Data.Text.Encoding      (decodeUtf8)
import System.FilePath         ((</>), takeDirectory)
import System.Directory        (createDirectoryIfMissing, withCurrentDirectory)
import Text.Blaze.Html         (Html)

import qualified Data.Text.IO                     as Text
import qualified System.FilePath.Glob             as Glob
import qualified Data.ByteString.Lazy             as ByteString
import qualified Text.Blaze.Html.Renderer.String  as BlazeString
import qualified System.FilePath                  as Path
import qualified System.Directory                 as Directory
import qualified System.Process                   as Process

import           Achille.Config
import           Achille.Writable (Writable)
import           Achille.Timestamped
import qualified Achille.Writable as Writable
import           Achille.Internal hiding (currentDir)
import qualified Achille.Internal as Internal


data Color = Red | Blue

color :: Color -> String -> String
color c x = "\x1b[" <> start <> x <> "\x1b[0m"
    where start = case c of
                      Red  -> "31m"
                      Blue -> "34m"

ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . takeDirectory

safeCopyFile :: FilePath -> FilePath -> IO ()
safeCopyFile from to = ensureDirExists to >> Directory.copyFile from to

-------------------------------------
-- Recipe building blocks (uncached)
-------------------------------------

-- | Recipe returning its input.
getInput :: Applicative m
         => Recipe m a a
getInput = nonCached $ pure . inputValue

-- | Recipe for retrieving the current directory
getCurrentDir :: Applicative m
              => Recipe m a FilePath
getCurrentDir = nonCached (pure . Internal.currentDir)

-- | Recipe retrieving the contents of the input file as text
readText :: MonadIO m
         => Recipe m FilePath Text
readText = nonCached \Context{..} ->
    liftIO $ Text.readFile (inputDir </> currentDir </> inputValue)

-- | Recipe for saving a value to the location given as input.
--   Returns the input filepath as is.
saveFile :: (MonadIO m, Writable a)
         => a -> Recipe m FilePath FilePath
saveFile = saveFileAs id

-- | Recipe for saving a value to a file, using the path modifier applied to the input filepath.
--   Returns the path of the output file.
saveFileAs :: (MonadIO m, Writable a)
           => (FilePath -> FilePath) -> a -> Recipe m FilePath FilePath
saveFileAs mod x = flip write x =<< mod <$> getInput

-- | Recipe for copying an input file to the same location in the output dir.
copyFile :: MonadIO m
         => Recipe m FilePath FilePath
copyFile = copyFileAs id

-- | Recipe for copying an input file to the output dir, using the path modifier.
copyFileAs :: MonadIO m
           => (FilePath -> FilePath) -> Recipe m FilePath FilePath
copyFileAs mod = getInput >>= \from -> copy from (mod from)

-- | Recipe for copying a file to a given destination.
--   Returns the output filepath.
copy :: MonadIO m
     => FilePath -> FilePath -> Recipe m a FilePath
copy from to = nonCached \Context{..} -> liftIO do
    safeCopyFile (inputDir  </> currentDir </> from)
                 (outputDir </> currentDir </> to)
    putStrLn (color Blue $ (currentDir </> from) <> " → " <> (currentDir </> to))
    pure to

-- | Recipe for writing to a an output file.
--   Returns the output filepath.
write :: MonadIO m
      => Writable b => FilePath -> b -> Recipe m a FilePath
write to x = nonCached \Context{..} -> liftIO do
    ensureDirExists (outputDir </> currentDir </> to)
    Writable.write (outputDir  </> currentDir </> to) x
    putStrLn (color Blue $ "writing " <> (currentDir </> to))
    pure to

-- | Make a recipe out of a task. The input will simply be discarded.
task :: Task m b -> Recipe m a b
task (Recipe r) = Recipe (r . void)



------------------------------
-- Lifted IO
------------------------------

-- | Recipe for printing a value to the console.
debug :: MonadIO m
      => Show b => b -> Recipe m a ()
debug = liftIO . putStrLn . show

callCommand :: MonadIO m
            => String -> Recipe m a ()
callCommand = liftIO . Process.callCommand

runCommandWith :: MonadIO m
               => (FilePath -> FilePath)
               -> (FilePath -> FilePath -> String)
               -> Recipe m FilePath FilePath
runCommandWith mod cmd = nonCached \Context{..} -> liftIO do
    let p' = mod inputValue
    Process.callCommand $ cmd (inputDir </> currentDir </> inputValue)
                              (outputDir </> currentDir </> p')
    pure p'


toTimestamped :: Monad m => FilePath -> Recipe m a (Timestamped FilePath)
toTimestamped = pure . timestamped
