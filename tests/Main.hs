{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.HUnit

import FakeIO
import Achille.Recipe
import Achille.Internal
import System.FilePath
import qualified Data.Map.Strict      as M
import qualified Data.ByteString.Lazy as LBS
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
            (Just "somewhere.txt", [ WrittenFile "output/somewhere.txt" "somestuff" ])

    , testCase "Read text from the fs" $
          exactRun fs (testCtx "fichier.txt")
            (readText)
            (Just "helloworld", [ HasReadFile "content/fichier.txt" ])

    , testCase "Read text and write it to the fs" $
          exactRun fs (testCtx "fichier.txt")
            (readText >>= saveFile)
            (Just "fichier.txt", [ HasReadFile "content/fichier.txt"
                            , WrittenFile "output/fichier.txt" "helloworld" ])
    , testCase "Copy file" $
          exactRun fs (testCtx ())
            (copy "un.txt" "deux.txt")
            (Just "deux.txt", [ CopiedFile  "content/un.txt" "output/deux.txt" ])
    ]

