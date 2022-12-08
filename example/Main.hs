module Main where

import Prelude.Linear
import Achille as A

main :: IO ()
main = achille rules

rules :: Task IO ()
rules = task A.do
  debug "echo 1"

  match "posts/*.md" \src -> A.do
    (src, src') <- copy src
    debug "rendering post"
    debug src
    src'

  debug "echo 2"
