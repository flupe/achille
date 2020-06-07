{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}

module Recipe
    ( Recipe(Recipe)
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
    , toItem
    , (<?>)
    ) where

import Prelude hiding (read)

import Data.Binary      (Binary, encodeFile)
import System.FilePath
import System.Directory (copyFile, createDirectoryIfMissing, withCurrentDirectory)
import Data.Text        (Text, pack)
import Text.Pandoc
import Text.Blaze.Html  (Html)
import Codec.Picture (Image, DynamicImage(..), PixelRGB8, convertRGB8)
import Data.Dates.Formats (parseDateFormat)

import qualified Data.Text.IO                    as Text
import qualified System.FilePath.Glob            as Glob
import qualified Data.ByteString.Lazy            as ByteString
import qualified Codec.Picture                   as JuicyPixels
import qualified Text.Blaze.Html.Renderer.String as BlazeString
import qualified Text.Blaze.Html.Renderer.Utf8 (renderHtmlToByteStringIO)


import Config
import Writable (Writable)
import qualified Writable
import Item


-- | type of recipe for cooking some b given an input a
newtype Recipe a b = Recipe { unRecipe :: a -> IO b }

instance Functor (Recipe a) where
    fmap f (Recipe x) = Recipe \a -> fmap f (x a)

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
getInput = Recipe (pure . id)

-- | Recipe for executing any IO action
liftIO :: IO b -> Recipe a b
liftIO x = Recipe (const x)

-- | Recipe for retrieving the text of the input file
read :: Recipe FilePath Text
read = Recipe (Text.readFile . (contentDir </>))

-- | Recipe for saving the current value to the same location as the input file
save :: Writable a => a -> Recipe FilePath FilePath
save = saveTo id

-- | Recipe for saving the current value to the output dir,
--   using the path modifier
saveTo :: Writable a => (FilePath -> FilePath) -> a -> Recipe FilePath FilePath
saveTo mod x = Recipe \p -> do
    let p' = outputDir </> mod p
    ensureDirExists p'
    Writable.write p' x
    pure (mod p)

-- | Recipe for copying an input file to the same location in the output dir
copy :: Recipe FilePath FilePath
copy = copyTo id

-- | Recipe for copying an input file in the output dir,
--   using the path modifier
copyTo :: (FilePath -> FilePath) -> Recipe FilePath FilePath
copyTo mod = Recipe \p -> safeCopyFile (contentDir </> p) (outputDir </> mod p)
                       >> pure (mod p)

-- | Recipe for loading a pandoc document
readPandoc :: Recipe FilePath Pandoc
readPandoc = readPandocWith def

-- | Recipe for loading a pandoc document using a given reader config
readPandocWith :: ReaderOptions -> Recipe FilePath Pandoc
readPandocWith ropts = Recipe \p ->
    let ext = drop 1 $ takeExtension p
        Just reader = lookup (pack ext) readers
    in case reader of
        ByteStringReader f ->
            ByteString.readFile (contentDir </> p)
            >>= runIOorExplode <$> f ropts
        TextReader f ->
            Text.readFile (contentDir </> p)
            >>= runIOorExplode <$> f ropts

-- | Recipe for loading an image using the input path
readImage :: Recipe FilePath (Image PixelRGB8)
readImage = Recipe \p -> JuicyPixels.readImage (contentDir </> p) >>= \case
    Left e    -> fail e
    Right img -> pure $ convertRGB8 img


--------------------------
-- Lifted IO


-- | Recipe for writing to a file
write :: Writable b => FilePath -> b -> Recipe a ()
write p x = liftIO $ ensureDirExists (outputDir </> p) >> Writable.write (outputDir </> p) x

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

toItem :: FilePath -> Recipe a (Item FilePath)
toItem x = liftIO $ case parseDateFormat "YYYY-MM-DD" (takeFileName x) of
    Left  e -> fail $ "Unable to read date from " <> x
    Right d -> pure $ Item d x

-- | Helper to log a message when a recipe is ran
(<?>) :: Recipe a b -> String -> Recipe a b
r <?> msg = liftIO (putStrLn msg) >> r


-- I know, this has nothing to do here
instance Show Html where
    show = BlazeString.renderHtml
