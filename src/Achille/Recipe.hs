{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs           #-}

module Achille.Recipe
    ( Recipe
    , getInput
    , liftIO
    , readText
    , save
    , saveTo
    , copy
    , copyTo
    , write
    , task
    , currentDir
    , debug
    , callCommand
    , runCommandWith
    , logInput
    , logInputWith
    , toTimestamped
    ) where

import Prelude hiding (read)

import Data.Binary        (Binary, encodeFile)
import Data.Functor       (void)
import Data.Text          (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import System.FilePath    ((</>), takeDirectory)
import System.Directory   (copyFile, createDirectoryIfMissing, withCurrentDirectory)
import Text.Blaze.Html    (Html)

import qualified Data.Text.IO                     as Text
import qualified System.FilePath.Glob             as Glob
import qualified Data.ByteString.Lazy             as ByteString
import qualified Text.Blaze.Html.Renderer.String  as BlazeString
import qualified System.FilePath                  as Path
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
safeCopyFile from to = ensureDirExists to >> copyFile from to
                    >> putStrLn (color Blue $ from <> " → " <> to)

------------------------------
-- Recipe building blocks
------------------------------

-- | Recipe returning its input.
getInput :: Recipe a a
getInput = nonCached $ pure . inputValue

-- | Recipe running an IO action.
liftIO :: IO b -> Recipe a b
liftIO x = nonCached $ const x

-- | Recipe retrieving the text of the input file.
readText :: Recipe FilePath Text
readText = nonCached \Context{..} -> Text.readFile (inputDir </> currentDir </> inputValue)

-- | Recipe for saving the current value to the same location as the input file.
save :: Writable a => a -> Recipe FilePath FilePath
save = saveTo id

-- | Recipe for saving the current value to the output dir,
--   using the path modifier.
saveTo :: Writable a => (FilePath -> FilePath) -> a -> Recipe FilePath FilePath
saveTo mod x = nonCached \Context{..} -> do
    let p'  = mod inputValue
    let out = outputDir </> currentDir </> p'
    ensureDirExists out
    Writable.write out x
    pure p'

-- | Recipe for copying an input file to the same location in the output dir.
copy :: Recipe FilePath FilePath
copy = copyTo id

-- | Recipe for copying an input file in the output dir,
--   using the path modifier.
copyTo :: (FilePath -> FilePath) -> Recipe FilePath FilePath
copyTo mod = nonCached \Context{..} ->
    safeCopyFile (inputDir </> currentDir </> inputValue)
                 (outputDir </> currentDir </> mod inputValue)
        >> pure (mod inputValue)

-- | Recipe for writing to a file
write :: Writable b => FilePath -> b -> Recipe a FilePath
write p x = nonCached \Context{..} ->
    ensureDirExists (outputDir </> currentDir </> p)
        >> Writable.write (outputDir </> currentDir </> p) x
        >> putStrLn (color Blue $ "writing " <> (outputDir </> currentDir </> p))
        >> pure p

-- | Make a recipe out of a task. The input will simply be discarded.
task :: Task b -> Recipe a b
task (Recipe r) = Recipe (r . void)


currentDir :: Recipe a FilePath
currentDir = nonCached (pure . Internal.currentDir)


------------------------------
-- Lifted IO
------------------------------

-- | Recipe for printing a value to the console.
debug :: Show b => b -> Recipe a ()
debug = liftIO . putStrLn . show

callCommand :: String -> Recipe a ()
callCommand = liftIO . Process.callCommand

runCommandWith :: (FilePath -> FilePath)
               -> (FilePath -> FilePath -> String)
               -> Recipe FilePath FilePath
runCommandWith mod cmd = nonCached \Context{..} -> do
    let p' = mod inputValue
    Process.callCommand $ cmd (inputDir </> currentDir </> inputValue)
                              (outputDir </> currentDir </> p')
    pure p'

logInput :: Show a => Recipe a ()
logInput = getInput >>= debug

logInputWith :: (Show b) => (a -> b) -> Recipe a ()
logInputWith f = (f <$> getInput) >>= debug

toTimestamped :: FilePath -> Recipe a (Timestamped FilePath)
toTimestamped = liftIO . pure . timestamped
