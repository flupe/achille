module Main where

import Prelude.Linear
import Achille

main :: IO ()
main = achille rules

rules :: Task IO ()
rules = task Achille.do
  void $ match "posts/*.md" debug
