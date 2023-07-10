{-# LANGUAGE QualifiedDo, BlockArguments, OverloadedStrings, RebindableSyntax #-}
module Test.Achille.Caching where

import Prelude hiding (log, fail, (>>=), (>>))
import qualified Prelude
import Data.Text (Text)
import Data.Function ((&))
import Control.Monad.State (modify)
import qualified Data.Map as Map

import Data.String
import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.Common
import Test.Achille.FakeIO

import Achille as A

tests :: TestTree
tests = testGroup "cached combinator"

  [ testCase "cached combinator does cache" $ testRun

      (cached A.do
        readText "fichier.txt"
          & write "output.txt"
        pure ())

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
            ]
          )

  , testCase "cached combinator correct w.r.t dynamic dependencies" $ testRun

      (cached A.do
        readText "fichier.txt"
          & write "output.txt"
        pure ())

      Prelude.do

        buildAndExpect
          ( Just ()
          , [ CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            , WrittenFile "output/output.txt" "helloworld"
            ]
          )

        waitASec

        -- TODO(flupe): make a nicer testing API
        modify \ s -> s { tFS = Map.insert "content/fichier.txt"
          (File (tCurrentTime s) "hello") (tFS s) }

        buildAndExpect
          ( Just ()
          , [ CheckedFile "content/fichier.txt"
            , CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            , WrittenFile "output/output.txt" "hello"
            ]
          )

  , testCase "cached combinator correct w.r.t environment variables" $ testRun

      A.do
        x <- readText "fichier.txt"
        y <- readText "post.md"
        cached $ write "output.txt" x
        pure ()

      Prelude.do

        buildAndExpect
          ( Just ()
          , [ CheckedMTime "content/fichier.txt"
            , HasReadFile "content/fichier.txt"
            , CheckedMTime "content/post.md"
            , HasReadFile "content/post.md"
            , WrittenFile "output/output.txt" "helloworld"
            ]
          )

        waitASec

        buildAndExpect
          ( Just ()
          , [ CheckedFile "content/fichier.txt"
            , CheckedMTime "content/fichier.txt"
            , CheckedFile "content/post.md"
            , CheckedMTime "content/post.md"
            , HasReadFile "content/fichier.txt"
            , HasReadFile "content/post.md"
            ]
          )

        waitASec

        -- TODO(flupe): make a nicer testing API
        modify \ s -> s { tFS = Map.insert "content/fichier.txt"
          (File (tCurrentTime s) "hello") (tFS s) }

        buildAndExpect
          ( Just ()
          , [ CheckedFile "content/fichier.txt"
            , CheckedMTime "content/fichier.txt"
            , CheckedFile "content/post.md"
            , CheckedMTime "content/post.md"
            , HasReadFile "content/fichier.txt"
            , HasReadFile "content/post.md"
            , WrittenFile "output/output.txt" "hello"
            ]
          )

        waitASec

        -- TODO(flupe): make a nicer testing API
        modify \ s -> s { tFS = Map.insert "content/post.md"
          (File (tCurrentTime s) "nope") (tFS s) }

        buildAndExpect
          ( Just ()
          , [ CheckedFile "content/fichier.txt"
            , CheckedMTime "content/fichier.txt"
            , CheckedFile "content/post.md"
            , CheckedMTime "content/post.md"
            , HasReadFile "content/fichier.txt"
            , HasReadFile "content/post.md"
            ]
          )
  ]
