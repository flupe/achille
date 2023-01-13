{-# LANGUAGE OverloadedStrings, QualifiedDo, BlockArguments #-}
module Main where

import Prelude hiding ((>>=))
import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Runners.Reporter qualified as Reporter
import Test.Tasty.HUnit

import FakeIO
import Data.Map.Strict      qualified as Map
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)

import Achille qualified as A
import Achille.Path
import Achille.Recipe (Context(..))
import Achille.Task

fs :: FileSystem
fs = Map.fromList
  [ ("content" </> "fichier.txt", File defMTime "helloworld")

  , ("content" </> "dir1" </> "index.md", File defMTime "somecontent")
  , ("content" </> "dir1" </> "meta.md", File defMTime "metadata of dir1")
  , ("content" </> "dir2" </> "index.md", File defMTime "some content again")
  , ("content" </> "dir2" </> "meta.md", File defMTime "metadata of dir2")
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
main = defaultMainWithIngredients [Reporter.ingredient] tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "write" $
      exactRun fs testCtx
        A.do write "somewhere.txt" ("somestuff" :: Task FakeIO Text)
        ( Just "/somewhere.txt"
        , [ WrittenFile "output/somewhere.txt" "somestuff" ])

  , testCase "read" $
      exactRun fs testCtx
        A.do readText "fichier.txt"
        ( Just "helloworld"
        , [ CheckedMTime "content/fichier.txt"
          , HasReadFile "content/fichier.txt"
          ]
        )

  , testCase "readwrite" $
      exactRun fs testCtx
        A.do readText "fichier.txt" >>= write "fichier.txt"
        ( Just "/fichier.txt"
        , [ CheckedMTime "content/fichier.txt"
          , HasReadFile "content/fichier.txt"
          , WrittenFile "output/fichier.txt" "helloworld"
          ]
        )

  , testCase "copy" $
      exactRun fs testCtx
        A.do copy "un.txt"
        ( Just "/un.txt"
        , [ CheckedMTime "content/un.txt", CopiedFile  "content/un.txt" "output/un.txt" ]
        )

--  , testCase "nestedmatch" $
--      exactRun fs testCtx
--        A.do
--          void $ match "*/index.md" \src ->
--            void $ match "meta.md" copy
--        (Just ()
--        , [ CopiedFile "content/dir1/meta.md" "content/dir1/meta.md"
--          , CopiedFile "content/dir2/meta.md" "content/dir2/meta.md"
--          ]
--        )
  ]

