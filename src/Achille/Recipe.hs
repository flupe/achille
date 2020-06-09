{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs           #-}

module Achille.Recipe
    ( Recipe
    , Context(Context)
    , getInput
    , liftIO
    , read
    , save
    , saveTo
    , copy
    , copyTo
    , readPandoc
    , readPandocWith
    , readImage
    , write
    , debug
    , renderPandoc
    , renderPandocWith
    , compilePandoc
    , toTimestamped
    ) where

import Prelude hiding (read)

import Data.Binary      (Binary, encodeFile)
import System.FilePath
import System.Directory (copyFile, createDirectoryIfMissing, withCurrentDirectory)
import Data.Text        (Text, pack)
import Text.Pandoc
import Text.Blaze.Html  (Html)
import Codec.Picture (Image, DynamicImage(..), PixelRGB8, convertRGB8)

import qualified Data.Text.IO                    as Text
import qualified System.FilePath.Glob            as Glob
import qualified Data.ByteString.Lazy            as ByteString
import qualified Codec.Picture                   as JuicyPixels
import qualified Text.Blaze.Html.Renderer.String as BlazeString
import qualified Text.Blaze.Html.Renderer.Utf8 (renderHtmlToByteStringIO)

import           Achille.Config
import           Achille.Writable (Writable)
import           Achille.Timestamped
import qualified Achille.Writable as Writable
import           Achille.Internal (Context(..), Recipe(RunIO))



ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . takeDirectory

safeCopyFile :: FilePath -> FilePath -> IO ()
safeCopyFile from to = ensureDirExists to >> copyFile from to


------------------------------
-- Recipe building blocks
------------------------------

-- | Recipe for retrieving the input
getInput :: Recipe a a
getInput = RunIO (pure . inputValue)

-- | Recipe for executing any IO action
liftIO :: IO b -> Recipe a b
liftIO x = RunIO (const x)

-- | Recipe for retrieving the text of the input file
read :: Recipe FilePath Text
read = RunIO \(Context inputDir _ p) ->
    Text.readFile (inputDir </> p)

-- | Recipe for saving the current value to the same location as the input file
save :: Writable a => a -> Recipe FilePath FilePath
save = saveTo id

-- | Recipe for saving the current value to the output dir,
--   using the path modifier
saveTo :: Writable a => (FilePath -> FilePath) -> a -> Recipe FilePath FilePath
saveTo mod x = RunIO \(Context inputDir outputDir p) -> do
    let p'  = mod p
    let out = outputDir </> p'
    ensureDirExists out
    Writable.write out x
    pure p'

-- | Recipe for copying an input file to the same location in the output dir
copy :: Recipe FilePath FilePath
copy = copyTo id

-- | Recipe for copying an input file in the output dir,
--   using the path modifier
copyTo :: (FilePath -> FilePath) -> Recipe FilePath FilePath
copyTo mod = RunIO \(Context inputDir outputDir p) ->
    safeCopyFile (inputDir </> p) (outputDir </> mod p)
        >> pure (mod p)

-- | Recipe for loading a pandoc document
readPandoc :: Recipe FilePath Pandoc
readPandoc = readPandocWith def


-- | Recipe for loading a pandoc document using a given reader config
readPandocWith :: ReaderOptions -> Recipe FilePath Pandoc
readPandocWith ropts = RunIO \(Context inputDir _ p) ->
    let ext = drop 1 $ takeExtension p
        Just reader = lookup (pack ext) readers
    in case reader of
        ByteStringReader f ->
            ByteString.readFile (inputDir </> p)
                >>= runIOorExplode <$> f ropts
        TextReader f ->
            Text.readFile (inputDir </> p)
                >>= runIOorExplode <$> f ropts

-- | Recipe for loading an image using the input path
readImage :: Recipe FilePath (Image PixelRGB8)
readImage = RunIO \(Context inputDir _ p) ->
    JuicyPixels.readImage (inputDir </> p) >>= \case
        Left e    -> fail e
        Right img -> pure $ convertRGB8 img


-- | Recipe for writing to a file
write :: Writable b => FilePath -> b -> Recipe a ()
write p x = RunIO \(Context _ outputDir _) ->
    ensureDirExists (outputDir </> p)
        >> Writable.write (outputDir </> p) x


------------------------------
-- Lifted IO
------------------------------

-- | Recipe for printing a value to the console
debug :: Show b => b -> Recipe a ()
debug = liftIO . print

-- | Recipe to convert a Pandoc document to Html
renderPandoc :: Pandoc -> Recipe a Html
renderPandoc = renderPandocWith def 

renderPandocWith :: WriterOptions -> Pandoc -> Recipe a Html
renderPandocWith wopts = liftIO . runIOorExplode <$> writeHtml5 wopts

compilePandoc :: Recipe FilePath Html
compilePandoc = readPandoc >>= renderPandoc

toTimestamped :: FilePath -> Recipe a (Timestamped FilePath)
toTimestamped = liftIO . pure . timestamped
