{-# LANGUAGE QualifiedDo, BlockArguments, OverloadedStrings #-}
module Test.Achille.ReadWrite where

import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.Common
import Test.Achille.FakeIO

import Achille as A

tests :: TestTree
tests = testGroup "read/write tests"
  [ testCase "write" $ exactRun

      -- Writing the file will do just that, and return a URL pointing to the file

      A.do write "somewhere.txt" ("somestuff" :: Task FakeIO Text)

      ( Just "/somewhere.txt"
      , [ WrittenFile "output/somewhere.txt" "somestuff" ])

  , testCase "read" $ exactRun

      -- Reading the file will do just that, but also check its modification time to know 
      -- if it has changed

      A.do readText "fichier.txt"

      ( Just "helloworld"
      , [ CheckedMTime "content/fichier.txt"
        , HasReadFile "content/fichier.txt"
        ])

  , testCase "readwrite" $ exactRun

      A.do readText "fichier.txt" A.>>= write "fichier.txt"

      ( Just "/fichier.txt"
      , [ CheckedMTime "content/fichier.txt"
        , HasReadFile "content/fichier.txt"
        , WrittenFile "output/fichier.txt" "helloworld"
        ])

  , testCase "readwrite unchanged" $ testRun

      A.do readText "fichier.txt" A.>>= write "fichier.txt"

      do
        -- first run, the file is read and written to.

        buildAndExpect
          ( Just "/fichier.txt"
          , [ CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            , WrittenFile "output/fichier.txt" "helloworld"
            ]
          )

        waitASec

        -- second run the file is read (because readText doesn't cache)
        -- but NOT written to.
        --
        buildAndExpect
          ( Just "/fichier.txt"
          , [ CheckedFile "content/fichier.txt"
            , CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            ]
          )
  ]



