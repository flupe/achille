{-# LANGUAGE QualifiedDo, BlockArguments, OverloadedStrings, RebindableSyntax #-}
module Test.Achille.Branching where

import Prelude hiding (log, fail, (>>=))
import Data.Text (Text)

import Data.String
import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.Common
import Test.Achille.FakeIO

import Achille as A

tests :: TestTree
tests = testGroup "error recovery tests"

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
  ]
