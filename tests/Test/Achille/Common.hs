{-# LANGUAGE OverloadedStrings #-}
module Test.Achille.Common where

import Data.Map.Strict qualified as Map

import Achille.Path
import Achille.Config (defaultConfig, Config(outputDir))
import Achille.Context (Context(..))

import Test.Achille.FakeIO
