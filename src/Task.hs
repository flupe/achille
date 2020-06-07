{-# LANGUAGE GADTs          #-}
{-# LANGUAGE BlockArguments #-}

module Task where

import System.FilePath
import System.Directory
import Data.Typeable (Typeable)

import qualified System.FilePath.Glob as Glob

import Recipe
import Config


-- | Description of a task producing an intermediate value of type a
data Task a where
    TaskMatch :: Glob.Pattern -> Recipe FilePath a -> Task [a]
    TaskWith  :: a -> Recipe a b -> Task b
    TaskIO    :: IO a -> Task a
    TaskBind  :: Task a -> (a -> Task b) -> Task b


-- | Run a task as is, no incremental builds
runTask :: Task a -> IO a
runTask (TaskMatch p (Recipe r)) = 
    withCurrentDirectory contentDir (Glob.globDir1 p "")
        >>= mapM r
runTask (TaskWith x (Recipe r)) = r x
runTask (TaskBind ta f) = runTask ta >>= runTask . f
runTask (TaskIO x) = x


instance Functor Task where
    fmap f t@(TaskMatch p r) = TaskIO $ f <$> runTask t
    fmap f (TaskWith x r)    = TaskWith x (f <$> r)
    fmap f (TaskIO x)        = TaskIO $ f <$> x
    fmap f (TaskBind ta tb)  = TaskBind ta (fmap f . tb)


instance Applicative Task where
    pure x    = TaskWith () (pure x)
    tf <*> tx = TaskBind tf \f -> TaskBind tx \x -> pure (f x)


instance Monad Task where
    (>>=) = TaskBind


------------------
-- Task builders

match :: Glob.Pattern -> Recipe FilePath a -> Task [a]
match = TaskMatch

with :: a -> Recipe a b -> Task b
with = TaskWith
