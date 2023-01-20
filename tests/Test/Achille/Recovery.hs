{-# LANGUAGE QualifiedDo, BlockArguments, OverloadedStrings #-}
module Test.Achille.Recovery where

import Prelude hiding (log, fail, (>>=))
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.Common
import Test.Achille.FakeIO

import Achille as A

tests :: TestTree
tests = testGroup "error recovery tests"

  [ testCase "seq failure" $

    -- the first task fails, yet the second one will succeed

      exactRun baseFS baseCtx

        A.do
          fail "oops" >>= log
          log "hello"

        (Just (), [ Logged "hello" ])

  , testCase "seq failure" $

    -- and the ordering doesn't matter,
    -- though if the last one fails we get no result

      exactRun baseFS baseCtx

        A.do
          log "hello"
          fail "oops" >>= log

        (Nothing, [ Logged "hello" ])

  , testCase "pair failure" $

    -- Same thing for pairs
    -- either subtask can fail and the other succeed

      exactRun baseFS baseCtx

        A.do (fail "oops" >>= log) :*: log "ok"

        (Nothing, [ Logged "ok" ])
  ]
