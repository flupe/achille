{-# LANGUAGE BlockArguments, QualifiedDo, OverloadedStrings, OverloadedLists #-}
module Test.Achille.Misc where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.FakeIO
import Test.Achille.Common

import Achille as A

tests :: TestTree
tests = testGroup "misc tests"
  [ testCase "basic glob" $ exactRun
      A.do glob "*.md"
      ( Just ["other-post.md", "post.md"]
      , []
      )
  , testCase "overloaded lists and basic for" $ exactRun
      A.do
        for ["one.txt", "two.txt"] \src ->
          write src ("hello" :: Task FakeIO Text)

      ( Just ["/one.txt", "/two.txt"]
      , [ WrittenFile "output/one.txt" "hello"
        , WrittenFile "output/two.txt" "hello"
        ]
      )
  ]
