module Main where

import Prelude (IO)
import Achille as A

main :: IO ()
main = achille rules

rules :: Task IO ()
rules = recipe \_ -> A.do
  debug "echo 1"

  match "posts/*.md" \src -> A.do
    debug "rendering post"
    debug src
    src

  debug "echo 2"
