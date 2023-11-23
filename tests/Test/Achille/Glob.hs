{-# LANGUAGE BlockArguments, QualifiedDo, OverloadedStrings #-}
module Test.Achille.Glob where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.FakeIO
import Test.Achille.Common

import Achille as A

tests :: TestTree
tests = testGroup "glob tests"
  [ testCase "basic glob" $ exactRun
      A.do glob "*.md"
      ( Just ["other-post.md", "post.md"]
      , []
      )
  ]
