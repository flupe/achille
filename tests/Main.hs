{-# LANGUAGE OverloadedStrings, QualifiedDo, BlockArguments #-}
module Main where

import Prelude hiding ((>>=))
import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Runners.Reporter qualified as Reporter
import Test.Tasty.HUnit

import Data.Map.Strict      qualified as Map
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)

import Achille qualified as A
import Achille.Task

import Test.Achille.FakeIO
import Test.Achille.Common
import Test.Achille.Match     qualified as Match
import Test.Achille.ReadWrite qualified as ReadWrite
import Test.Achille.Recovery  qualified as Recovery


main :: IO ()
main = defaultMainWithIngredients [Reporter.ingredient] tests

tests :: TestTree
tests = testGroup "Tests"
  [ Match.tests
  , ReadWrite.tests
  , Recovery.tests
  , testCase "copy" $
      exactRun
        A.do copy "un.txt"
        ( Just "/un.txt"
        , [ CheckedMTime "content/un.txt", CopiedFile  "content/un.txt" "output/un.txt" ]
        )
  ]

