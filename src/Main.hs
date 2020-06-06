{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}

import Prelude hiding (readFile, read)
import Options.Applicative
import Data.Semigroup       ((<>))
import System.FilePath      ((</>), (-<.>), splitExtensions, addExtension)
import System.Process       (callCommand)
import System.Directory     (removePathForcibly)
import Codec.Picture        (dynamicPixelMap)
import Codec.Picture.Extra  (scaleBilinear)
import Data.Functor         ((<&>))

import Templates
import Recipe
import Config

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
   <> command "Clean"  (info (pure Clean)  (progDesc "Delete all artefacts"))


main :: IO ()
main = customExecParser p opts >>= runCommand
    where
        opts = info (cli <**> helper) $ fullDesc <> header description
        p    = prefs showHelpOnEmpty


runCommand :: Command -> IO ()
runCommand Deploy = callCommand deployCmd
runCommand Clean  = removePathForcibly outputDir

runCommand Build = do
    putStrLn "starting..."

    match "posts/*" $ getInput >>= debug

    match "visual/*/*" do
        copy -- we copy the actual image first

        readImage
            <&> scaleBilinear 40 40
            >>= saveTo (+<.> "thumb")

    with "index.rst" $ compilePandoc >>= saveTo (-<.> "html")
