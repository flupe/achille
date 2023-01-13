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
  [ testCase "write" $
      exactRun baseFS baseCtx
        A.do write "somewhere.txt" ("somestuff" :: Task FakeIO Text)
        ( Just "/somewhere.txt"
        , [ WrittenFile "output/somewhere.txt" "somestuff" ])

  , testCase "read" $
      exactRun baseFS baseCtx
        A.do readText "fichier.txt"
        ( Just "helloworld"
        , [ CheckedMTime "content/fichier.txt"
          , HasReadFile "content/fichier.txt"
          ]
        )

  , testCase "readwrite" $
      exactRun baseFS baseCtx
        A.do readText "fichier.txt" A.>>= write "fichier.txt"
        ( Just "/fichier.txt"
        , [ CheckedMTime "content/fichier.txt"
          , HasReadFile "content/fichier.txt"
          , WrittenFile "output/fichier.txt" "helloworld"
          ]
        )
  ]
