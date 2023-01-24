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

  [ testCase "seq failure first" $ exactRun

      -- the first task fails, yet the second one will succeed

      A.do
        fail "oops" >>= log
        log "hello"

      (Just (), [ Logged "hello" ])

  , testCase "seq failure last" $ exactRun

      -- and the ordering doesn't matter,
      -- though if the last one fails we get no result

      A.do
        log "hello"
        fail "oops" >>= log

      (Nothing, [ Logged "hello" ])

  , testCase "pair failure" $ exactRun

      -- Same thing for pairs
      -- either subtask can fail and the other succeed

      A.do (fail "oops" >>= log) :*: log "ok"

      (Nothing, [ Logged "ok" ])

  , testCase "bind failure unused" $ exactRun

      -- when a failing task is bound to a variable that isn't used
      -- by tasks that come next, the latter still succeed

      A.do
        x <- fail "oops"
        log "hello"

      (Just (), [ Logged "hello" ])

  , testCase "bind seq failure first" $ exactRun

      -- when a failing task is bound to a variable,
      -- only the tasks that depend on it will fail

      A.do
        x <- fail "oops"
        log x
        log "hello"

      (Just (), [ Logged "hello" ])

  , testCase "bind seq failure last" $ exactRun

      -- when a failing task is bound to a variable,
      -- only the tasks that depend on it will fail
      -- if the last one fails, we get no result

      A.do
        x <- fail "oops"
        log "hello"
        log x

      (Nothing, [ Logged "hello" ])
  ]
