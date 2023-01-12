{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding ((>>=))
import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.HUnit

import FakeIO
import Data.Map.Strict      qualified as Map
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)

import Achille.Path
import Achille.Recipe (Context(..))
import Achille.Task

fs :: FileSystem
fs = Map.fromList
  [ ("content" </> "fichier.txt", File defMTime "helloworld")
  ]

testCtx :: Context
testCtx = Context
  { lastTime = defMTime
  , inputRoot    = "content"
  , outputRoot   = "output"
  , updatedFiles = Map.empty
  , currentDir   = ""
  , sitePrefix   = ""
  , cleanBuild   = True
  }

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "write" $
      exactRun fs testCtx
        (write "somewhere.txt" ("somestuff" :: Task FakeIO Text))
        ( Just "/somewhere.txt"
        , [ WrittenFile "output/somewhere.txt" "somestuff" ])

  , testCase "read" $
      exactRun fs testCtx
        (readText "fichier.txt")
        ( Just "helloworld"
        , [ CheckedMTime "content/fichier.txt"
          , HasReadFile "content/fichier.txt"
          ]
        )

  , testCase "readwrite" $
      exactRun fs testCtx
        (readText "fichier.txt" >>= write "fichier.txt")
        ( Just "/fichier.txt"
        , [ CheckedMTime "content/fichier.txt"
          , HasReadFile "content/fichier.txt"
          , WrittenFile "output/fichier.txt" "helloworld"
          ]
        )

  , testCase "copy" $
      exactRun fs testCtx
        (copy "un.txt")
        ( Just "/un.txt"
        , [ CheckedMTime "content/un.txt", CopiedFile  "content/un.txt" "output/un.txt" ]
        )
  ]

