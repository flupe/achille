module Main where

import Achille

main :: IO ()
main = achille $ recipe $ Achille.do
  encode (debug "okok") unit
  encode (debug "okok") unit
