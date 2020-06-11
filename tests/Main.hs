{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.HUnit

import FakeIO
import Achille.Recipe
import Achille.Internal
import System.FilePath
import Data.Map.Strict      as M
import Data.ByteString.Lazy as LBS
import Data.Text (Text)

fs :: FileSystem
fs = M.fromList
    [ ("content" </> "fichier.txt",
          (defMTime, "helloworld"))
    ]

testCtx :: a -> Context a
testCtx x = Context
    { inputDir    = "content"
    , outputDir   = "output"
    , currentDir  = ""
    , timestamp   = defMTime
    , forceFiles  = []  
    , mustRun     = NoMust
    , cache       = LBS.empty
    , inputValue  = x
    }

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "Write on the fs and get the output path back:" $
          exactRun fs (testCtx ())
            (write "somewhere.txt" ("somestuff" :: Text))
            ("somewhere.txt", [ WrittenFile "output/somewhere.txt" ])

    , testCase "Read text from the fs" $
          exactRun fs (testCtx "fichier.txt")
            (readText)
            ("helloworld", [ HasReadFile "content/fichier.txt" ])
    ]

