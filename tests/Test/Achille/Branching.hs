{-# LANGUAGE QualifiedDo, BlockArguments, OverloadedStrings, RebindableSyntax #-}
module Test.Achille.Branching where

import Prelude hiding (log, fail, (>>=), (>>))
import qualified Prelude ((>>), (>>=))
import Data.Text (Text)

import Data.String
import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.Common
import Test.Achille.FakeIO

import Achille as A

tests :: TestTree
tests = testGroup "conditional branching tests"

  [ testCase "if true goes to the left" $ exactRun
      A.do
        if pure True then
          log "left"
        else
          log "right"
      (Just (), [ Logged "left" ])

  , testCase "if false goes to the right" $ exactRun
      A.do
        if pure False then
          log "left"
        else
          log "right"
      (Just (), [ Logged "right" ])

  , testCase "a branch is cached" $ testRun

      A.do

        if pure True then A.do
          write "output.txt" (readText "fichier.txt")
          pure ()
        else log "nope"

      Prelude.do

        buildAndExpect
          ( Just ()
          , [ CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            , WrittenFile "output/output.txt" "helloworld"
            ]
          )

        waitASec

        buildAndExpect
          ( Just ()
          , [ CheckedFile "content/fichier.txt"
            , CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            -- the input file is read again (for now) but nothing is written to disk,
            -- because nothing has changed
            ]
          )

  , testCase "branches have their own cache" $ testRun

      A.do

        if pure True then do
          write "output.txt" (readText "fichier.txt")
          pure ()
        else do
          write "output.txt" (readText "fichier.txt")
          log "nope"

      Prelude.do

        buildAndExpect
          ( Just ()
          , [ CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            , WrittenFile "output/output.txt" "helloworld"
            ]
          )

        waitASec

        buildAndExpect
          ( Just ()
          , [ CheckedFile "content/fichier.txt"
            , CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            -- the input file is read again (for now) but nothing is written to disk,
            -- because nothing has changed
            ]
          )
  ]
