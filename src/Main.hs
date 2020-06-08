{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}

import Data.String
import Options.Applicative
import Data.Semigroup       ((<>))
import System.FilePath      ((</>), (-<.>), splitExtensions, addExtension)
import System.Process       (callCommand)
import System.Directory     (removePathForcibly)
import Codec.Picture        (dynamicPixelMap)
import Codec.Picture.Extra  (scaleBilinear)
import Control.Monad
import Data.Functor         ((<&>))
import Data.Function        ((&))
import Data.List            (sort)
import Prelude hiding (readFile, read)
import Text.Pandoc.Options


import Templates
import Recipe hiding (compilePandoc)
import Config
import Task
import Item
import Thumbnail


compilePandoc = readPandoc >>= renderPandocWith wopts
    where wopts = def
            { writerHTMLMathMethod = KaTeX ""
            }

(+<.>) :: FilePath -> String -> FilePath
p +<.> e = let (n, es) = splitExtensions p in (addExtension n e) <> es

description :: String
description = "A static site generator for fun and profit"


data Command
    = Build   -- ^ Build the site once
    | Deploy  -- ^ Deploy to the server
    | Clean   -- ^ Delete all artefacts
    deriving (Eq, Show)


cli :: Parser Command
cli = subparser $
      command "build"  (info (pure Build)  (progDesc "Build the site once"))
   <> command "deploy" (info (pure Deploy) (progDesc "Server go brrr"))
   <> command "clean"  (info (pure Clean)  (progDesc "Delete all artefacts"))


main :: IO ()
main = customExecParser p opts >>= runCommand
    where
        opts = info (cli <**> helper) $ fullDesc <> header description
        p    = prefs showHelpOnEmpty


runCommand :: Command -> IO ()
runCommand Deploy = callCommand deployCmd
runCommand Clean  = removePathForcibly outputDir
                 >> removePathForcibly cacheFile
runCommand Build  = void $ achille build


build = do
    match "assets/theme.css" $ void copy

    match "./quid.rst" $ void $
        compilePandoc <&> outer >>= saveTo (-<.> "html")

    pictures <- match "visual/*/*" do
        copy
        readImage
            <&> downscaleToFit (FitWidth 740)
            >>= saveThumbnailTo (+<.> "thumb")

    with pictures $ match "./visual.rst" $ void do
        txt    <- compilePandoc
        write "visual.html" $ renderVisual txt pictures

    posts <- match "posts/*" do
        src <- copy
        compilePandoc
            <&> renderPost src
            >>= saveTo (-<.> "html")

    with posts $ match "./index.rst" $ void do
        compilePandoc
            <&> renderIndex posts
            >>= saveTo (-<.> "html")
