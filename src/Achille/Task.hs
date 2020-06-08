{-# LANGUAGE GADTs #-}

module Achille.Task where


import Control.Applicative   (liftA2)
import Control.Monad         (liftM, liftM2)
import Data.Binary           (Binary)
import System.FilePath       (FilePath)
import System.FilePath.Glob  (Pattern)

import Achille.Recipe
import Achille.Config


-- | Description of a task producing an intermediate value of type a
data Task a where
    TaskMatchVoid :: Pattern -> Recipe FilePath a -> Task ()
    TaskMatch     :: Binary a => Pattern -> Recipe FilePath a -> Task [a]
    TaskWith      :: (Binary a, Eq a, Binary b) => a -> Task b     -> Task b
    TaskIO        :: IO a -> Task a
    TaskBind      :: Task a -> (a -> Task b) -> Task b


instance Functor Task where
    fmap = liftM

instance Applicative Task where
    pure   = return
    liftA2 = liftM2

instance Monad Task where
    return = TaskIO . pure
    (>>=)  = TaskBind


match :: Binary a => Pattern -> Recipe FilePath a -> Task [a]
match = TaskMatch

match_ :: Pattern -> Recipe FilePath a -> Task ()
match_ = TaskMatchVoid

with :: (Binary a, Eq a, Binary b) => a -> Task b -> Task b
with = TaskWith
