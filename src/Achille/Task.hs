{-# LANGUAGE GADTs #-}

module Achille.Task
    ( Task(..)
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

import Achille.Recipe
import Achille.Config


-- | Description of a task producing an intermediate value of type a
data Task a where
    TaskMatchVoid :: Pattern -> Recipe FilePath a -> Task ()
    TaskMatch     :: Binary a => Pattern -> Recipe FilePath a -> Task [a]
    TaskWith      :: (Binary a, Eq a, Binary b) => a -> Task b -> Task b
    TaskRecipe    :: Recipe () a -> Task a
    TaskBind      :: Task a -> (a -> Task b) -> Task b
    TaskWatch     :: (Binary a, Eq a) => a -> Task b -> Task b


instance Functor Task where
    fmap = liftM

instance Applicative Task where
    pure   = return
    liftA2 = liftM2

instance Monad Task where
    return = TaskRecipe . liftIO . pure
    (>>=)  = TaskBind

always :: Recipe () a -> Task a
always = TaskRecipe

match :: Binary a => Pattern -> Recipe FilePath a -> Task [a]
match = TaskMatch

match_ :: Pattern -> Recipe FilePath a -> Task ()
match_ = TaskMatchVoid

with :: (Binary a, Eq a, Binary b) => a -> Task b -> Task b
with = TaskWith

watch :: (Binary a, Eq a) => a -> Task b -> Task b
watch = TaskWatch
