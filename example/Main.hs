module Main where

import Achille

main :: IO ()
main = achille rules

rules :: Task IO ()
rules = task Achille.do
  debug "build started"
  match "posts/*.md" debug
  debug "build finished"
