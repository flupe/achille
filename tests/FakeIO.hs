{-# LANGUAGE GADTs #-}

module FakeIO where


import Data.Time.Clock  (UTCTime)
import Data.Map.Strict as M
import Data.Maybe
import Data.Bifunctor (bimap)

import qualified System.Directory     as Directory
import qualified System.FilePath      as FilePath
import qualified System.FilePath.Glob as Glob
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.State

import Test.Tasty.HUnit

import Achille.Recipe
import Achille.Internal
import Achille.Internal.IO as AchilleIO


data FakeIO a where
    ReadFile            :: FilePath -> FakeIO BS.ByteString
    ReadFileLazy        :: FilePath -> FakeIO LBS.ByteString
    CopyFile            :: FilePath -> FilePath       -> FakeIO ()
    WriteFile           :: FilePath -> BS.ByteString  -> FakeIO ()
    WriteFileLazy       :: FilePath -> LBS.ByteString -> FakeIO ()
    CallCommand         :: String   -> FakeIO ()
    Log                 :: String   -> FakeIO ()
    Glob                :: FilePath -> Glob.Pattern -> FakeIO [FilePath]
    GetModificationTime :: FilePath -> FakeIO UTCTime

    SeqAp               :: FakeIO (a -> b) -> FakeIO a -> FakeIO b
    Fmap                :: (a -> b) -> FakeIO a -> FakeIO b
    Pure                :: a -> FakeIO a
    Bind                :: FakeIO a -> (a -> FakeIO b) -> FakeIO b


instance Functor FakeIO where
    fmap = Fmap

instance Applicative FakeIO where
    pure  = Pure
    (<*>) = SeqAp

instance Monad FakeIO where
    (>>=) = Bind


instance AchilleIO FakeIO where
    readFile            = ReadFile
    readFileLazy        = ReadFileLazy
    copyFile            = CopyFile
    writeFile           = WriteFile
    writeFileLazy       = WriteFileLazy
    callCommand         = CallCommand
    log                 = Log
    glob                = Glob
    getModificationTime = GetModificationTime


data FakeIOActions
    = WrittenFile          FilePath
    | WrittenFileLazy      FilePath
    | HasReadFile          FilePath
    | HasReadFileLazy      FilePath
    | CopiedFile FilePath  FilePath
    | CalledCommand        String
    deriving (Eq, Show)


type FileSystem = Map FilePath (UTCTime, BS.ByteString)

defMTime :: UTCTime
defMTime = undefined

getMTime :: FilePath -> FileSystem -> UTCTime
getMTime p fs = fromMaybe defMTime (fst <$> M.lookup p fs)

getBS :: FilePath -> FileSystem -> BS.ByteString
getBS p fs = fromMaybe BS.empty (snd <$> M.lookup p fs)

retrieveFakeIOActions :: FakeIO (a, Cache)    -- the fake IO computation
                      -> FileSystem  -- the underlying input FS
                      -> (a, [FakeIOActions])
retrieveFakeIOActions t fs = bimap fst reverse $ runState (retrieve t) []
    where
        retrieve :: FakeIO a -> State [FakeIOActions] a
        retrieve (ReadFile key) = do
            modify (HasReadFile key :)
            pure (getBS key fs)

        retrieve (ReadFileLazy key) = do
            modify (HasReadFileLazy key :)
            pure (LBS.fromStrict $ getBS key fs)

        retrieve (CopyFile from to)  = modify (CopiedFile from to :)
        retrieve (WriteFile p _)     = modify (WrittenFile p :)
        retrieve (WriteFileLazy p _) = modify (WrittenFile p :)
        retrieve (CallCommand cmd)   = modify (CalledCommand cmd :)
        retrieve (Log str)           = pure ()
        retrieve (Glob dir p)        = undefined
        retrieve (GetModificationTime p) = pure $ getMTime p fs

        retrieve (SeqAp f x) = do
            actions <- get
            let (f', actions')  = runState (retrieve f) actions
            let (x', actions'') = runState (retrieve x) actions'
            put actions''
            pure (f' x')

        retrieve (Fmap f x)  = do
            actions <- get
            let (x', actions') = runState (retrieve x) actions
            put actions'
            pure (f x')

        retrieve (Pure x)   = pure x

        retrieve (Bind x f) = do
            actions <- get
            let (x', actions') = runState (retrieve x) actions
            put actions'
            retrieve (f x')


exactRun :: (Show b, Eq b)
         => FileSystem
         -> Context a
         -> Recipe FakeIO a b
         -> (b, [FakeIOActions])
         -> Assertion
exactRun fs ctx r expected =
    let fakeIO = runRecipe r ctx
        trace  = retrieveFakeIOActions fakeIO fs
    in trace @?= expected
