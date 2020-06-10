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
    , readPandoc
    , readPandocWith
    , readImage
    , write
    , task
    , currentDir
    , debug
    , logInput
    , logInputWith
    , renderPandoc
    , renderPandocWith
    , compilePandoc
    , toTimestamped
    ) where

import Prelude hiding (read)

import Data.Binary      (Binary, encodeFile)
import Data.Functor     (void)
import Data.Text        (Text, pack)
import System.Directory (copyFile, createDirectoryIfMissing, withCurrentDirectory)
import Codec.Picture    (Image, DynamicImage(..), PixelRGB8, convertRGB8)
import Text.Blaze.Html  (Html)

import System.FilePath
import Text.Pandoc      hiding (nonCached)

import qualified Data.Text.IO                     as Text
import qualified System.FilePath.Glob             as Glob
import qualified Data.ByteString.Lazy             as ByteString
import qualified Codec.Picture                    as JuicyPixels
import qualified Text.Blaze.Html.Renderer.String  as BlazeString
import qualified System.FilePath                  as Path

import           Achille.Config
import           Achille.Writable (Writable)
import           Achille.Timestamped
import qualified Achille.Writable as Writable
import           Achille.Internal hiding (currentDir)
import qualified Achille.Internal as Internal



ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . takeDirectory

safeCopyFile :: FilePath -> FilePath -> IO ()
safeCopyFile from to = ensureDirExists to >> copyFile from to
    >> putStrLn ("copying from: " <> from <> " to: " <> to)

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

-- | Recipe for loading a pandoc document
readPandoc :: Recipe FilePath Pandoc
readPandoc = readPandocWith def


-- | Recipe for loading a pandoc document using a given reader config
readPandocWith :: ReaderOptions -> Recipe FilePath Pandoc
readPandocWith ropts = nonCached \Context{..}  ->
    let ext = drop 1 $ takeExtension inputValue
        Just reader = lookup (pack ext) readers
    in case reader of
        ByteStringReader f ->
            ByteString.readFile (inputDir </> currentDir </> inputValue)
                >>= runIOorExplode <$> f ropts
        TextReader f ->
            Text.readFile (inputDir </> currentDir </> inputValue)
                >>= runIOorExplode <$> f ropts

-- | Recipe for loading an image using the input path
readImage :: Recipe FilePath (Image PixelRGB8)
readImage = nonCached \Context{..} ->
    JuicyPixels.readImage (inputDir </> inputValue) >>= \case
        Left e    -> fail e
        Right img -> pure $ convertRGB8 img


-- | Recipe for writing to a file
write :: Writable b => FilePath -> b -> Recipe a ()
write p x = nonCached \Context{..} ->
    ensureDirExists (outputDir </> currentDir </> p)
        >> Writable.write (outputDir </> currentDir </> p) x

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

logInput :: Show a => Recipe a ()
logInput = getInput >>= debug

logInputWith :: (Show b) => (a -> b) -> Recipe a ()
logInputWith f = (f <$> getInput) >>= debug

-- | Recipe to convert a Pandoc document to Html.
renderPandoc :: Pandoc -> Recipe a Html
renderPandoc = renderPandocWith def 

renderPandocWith :: WriterOptions -> Pandoc -> Recipe a Html
renderPandocWith wopts = liftIO . runIOorExplode <$> writeHtml5 wopts

compilePandoc :: Recipe FilePath Html
compilePandoc = readPandoc >>= renderPandoc

toTimestamped :: FilePath -> Recipe a (Timestamped FilePath)
toTimestamped = liftIO . pure . timestamped
