{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DeriveFunctor  #-}

module Achille.Internal
    ( Context(..)
    , Recipe(..)
    , Task
    ) where


import Data.Binary           (Binary)
import Control.Monad         (liftM, liftM2)
import Control.Applicative   (liftA2)
import System.FilePath.Glob  (Pattern)


data Context a = Context
    { inputDir   :: FilePath
    , outputDir  :: FilePath
    , inputValue :: a
    } deriving (Functor)


-- | Description of a recipe producing an intermediate value a given some b.
data Recipe a b where
    -- | Encodes a recipe on files, discarding the result
    MatchVoid           -- Pattern for selecting files
                        :: Pattern 
                        -- Task run for every selected file
                        -> Recipe FilePath b
                        -> Task () 
    -- | Encodes a recipe on files, caching the result
    Match               :: Binary b
                        -- Pattern for selecting files
                        => Pattern
                        -- Task run for every selected file
                        -> Recipe FilePath b
                        -> Task [b]

    -- | Encodes a recipe ran on every directory.
    -- | Nothing is cached at the directory level.
    -- | The recipe is ran with the directory as current working dir.
    MatchDir            -- Pattern for selecting files
                        :: Pattern
                        -- Task run for every selected file
                        -> Recipe FilePath b
                        -> Task [b]


    -- | Encodes a task that will only be run when some value changes
    With                :: (Binary a, Eq a, Binary b)
                        => a       -- The value
                        -> Task b  -- The task
                        -> Task b
    -- | Encodes a task that is guaranteed to be recomputed when some value changes
    Watch               :: (Binary a, Eq a)
                        => a       -- The value
                        -> Task b  -- The task
                        -> Task b
    -- | Encodes a recipe doing IO
    RunIO               :: (Context a -> IO b) -> Recipe a b

    -- | Encodes a constant recipe
    Return              :: b -> Recipe a b

    -- | Encodes a sequence of tasks
    Bind                :: Recipe a b -> (b -> Recipe a c) -> Recipe a c


-- | A task is simply a recipe that expects no input
type Task = Recipe ()


instance Functor (Recipe a) where
    fmap = liftM

instance Applicative (Recipe a) where
    pure   = return
    liftA2 = liftM2

instance Monad (Recipe a) where
    return = Return
    (>>=)  = Bind
