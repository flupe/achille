{-# LANGUAGE BlockArguments, QualifiedDo, OverloadedStrings #-}
module Test.Achille.Match where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Achille.FakeIO
import Test.Achille.Common

import Achille as A

tests :: TestTree
tests = testGroup "match tests"
  [ testCase "basic match" $ exactRun baseFS baseCtx
      A.do match "*.md" \src -> readText src A.>>= write (src -<.> "html")
      ( Just ["/other-post.html", "/post.html"]
      , [ CheckedMTime "content/other-post.md"
        , CheckedMTime "content/other-post.md"
        , HasReadFile "content/other-post.md"
        , WrittenFile "output/other-post.html" "<strong>hello</strong>"

        , CheckedMTime "content/post.md"
        , CheckedMTime "content/post.md"
        , HasReadFile "content/post.md"
        , WrittenFile "output/post.html" "<em>hello</em>"
        ]
      )

  , testCase "nested match" $
      exactRun baseFS baseCtx
        A.do
          void $ match "*/index.md" \src ->
            void $ match "meta.md" copy
        ( Just ()
        , [ CheckedMTime "content/dir1/index.md"
          , CheckedMTime "content/dir1/meta.md"
          , CheckedMTime "content/dir1/meta.md"
          , CopiedFile "content/dir1/meta.md" "output/dir1/meta.md"

          , CheckedMTime "content/dir2/index.md"
          , CheckedMTime "content/dir2/meta.md"
          , CheckedMTime "content/dir2/meta.md"
          , CopiedFile "content/dir2/meta.md" "output/dir2/meta.md"
          ]
        )
  ]