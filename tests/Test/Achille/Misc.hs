{-# LANGUAGE BlockArguments, QualifiedDo, OverloadedStrings, OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Test.Achille.Misc where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.FakeIO
import Test.Achille.Common
import GHC.Generics qualified as GHC
import Generics.SOP

import Achille as A
import Achille.Diffable (Lifted(Lifted))
import Achille.Task (Pattern(..))

-- custom datatypes
data MyEither a b = MyLeft a | MyRight b
  deriving GHC.Generic
  deriving Generic

-- boilerplate that could be generated automatically

type MyEither_ a b = Lifted (MyEither a b)

pattern MyLeft_ :: Task m a -> Pattern m (MyEither a b)
pattern MyLeft_  x <- Pattern (Z (x :* Nil))

pattern MyRight_ :: Task m b -> Pattern m (MyEither a b)
pattern MyRight_ y <- Pattern (S (Z (y :* Nil)))

-----------------------------------------------------

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

  , testCase "basic switch left" $ exactRun
      A.do
        v :: Task FakeIO (MyEither_ Int Bool)
          <- pure (Lifted (MyLeft 3))
        switch v \case
          MyLeft_  x -> x
          MyRight_ y -> pure 5
      ( Just 3 , [])

  , testCase "basic switch right" $ exactRun
      A.do
        v :: Task FakeIO (MyEither_ Int Bool)
          <- pure (Lifted (MyRight True))
        switch v \case
          MyLeft_  x -> x
          MyRight_ y -> pure 5
      ( Just 5
      , []
      )
  ]
