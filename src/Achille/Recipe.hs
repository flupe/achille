{-# LANGUAGE DeriveFunctor #-}

module Achille.Recipe
    ( Recipe(Recipe)
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

import Achille.Config
import Achille.Writable (Writable)
import qualified Achille.Writable as Writable
import Achille.Timestamped


data Context a = Context
    { inputDir   :: FilePath
    , outputDir  :: FilePath
    , inputValue :: a
    } deriving (Functor)


-- | Type of recipes for cooking some b given an input a and a working directory
newtype Recipe a b = Recipe { unRecipe :: Context a -> IO b }

instance Functor (Recipe a) where
    fmap f (Recipe r) = Recipe \a -> fmap f (r a)

instance Applicative (Recipe a) where
    pure x  = Recipe (const (pure x))
    (Recipe f) <*> (Recipe x) = Recipe
        \a -> f a <*> x a

instance Monad (Recipe a) where
    (Recipe x) >>= f = Recipe
        \a -> (x a >>= \x -> unRecipe (f x) a)


ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . takeDirectory

safeCopyFile :: FilePath -> FilePath -> IO ()
safeCopyFile from to = ensureDirExists to >> copyFile from to


------------------------------
-- Recipe building blocks
------------------------------

-- | Recipe for retrieving the input
getInput :: Recipe a a
getInput = Recipe (pure . inputValue)

-- | Recipe for executing any IO action
liftIO :: IO b -> Recipe a b
liftIO x = Recipe (const x)

-- | Recipe for retrieving the text of the input file
read :: Recipe FilePath Text
read = Recipe \(Context inputDir _ p) ->
    Text.readFile (inputDir </> p)

-- | Recipe for saving the current value to the same location as the input file
save :: Writable a => a -> Recipe FilePath FilePath
save = saveTo id

-- | Recipe for saving the current value to the output dir,
--   using the path modifier
saveTo :: Writable a => (FilePath -> FilePath) -> a -> Recipe FilePath FilePath
saveTo mod x = Recipe \(Context inputDir outputDir p) -> do
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
copyTo mod = Recipe \(Context inputDir outputDir p) ->
    safeCopyFile (inputDir </> p) (outputDir </> mod p)
        >> pure (mod p)

-- | Recipe for loading a pandoc document
readPandoc :: Recipe FilePath Pandoc
readPandoc = readPandocWith def

-- | Recipe for loading a pandoc document using a given reader config
readPandocWith :: ReaderOptions -> Recipe FilePath Pandoc
readPandocWith ropts = Recipe \(Context inputDir _ p) ->
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
readImage = Recipe \(Context inputDir _ p) ->
    JuicyPixels.readImage (inputDir </> p) >>= \case
        Left e    -> fail e
        Right img -> pure $ convertRGB8 img


-- | Recipe for writing to a file
write :: Writable b => FilePath -> b -> Recipe a ()
write p x = Recipe \(Context _ outputDir _) ->
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
