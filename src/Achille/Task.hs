{-# LANGUAGE GADTs #-}

module Achille.Task
    ( Task
    , match
    , match_
    , with
    , watch
    ) where


import Control.Applicative   (liftA2)
import Control.Monad         (liftM, liftM2)
import Data.Binary           (Binary)
import System.FilePath       (FilePath)
import System.FilePath.Glob  (Pattern)

import Achille.Internal (Recipe(..), Task)


match :: Binary a => Pattern -> Recipe FilePath a -> Task [a]
match = Match

matchDir :: Pattern -> Recipe FilePath a -> Task [a]
matchDir = MatchDir

match_ :: Pattern -> Recipe FilePath a -> Task ()
match_ = MatchVoid

with :: (Binary a, Eq a, Binary b) => a -> Task b -> Task b
with = With

watch :: (Binary a, Eq a) => a -> Task b -> Task b
watch = Watch
