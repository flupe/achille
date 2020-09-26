{-# LANGUAGE GADTs #-}

module FakeIO where


import Data.Time.Clock  (UTCTime)
import Data.Map.Strict as M
import Data.Maybe
import Data.Bifunctor     (bimap)
import Control.Monad.Fail (MonadFail, fail)

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
    DoesFileExist       :: FilePath -> m Bool
    DoesDirExist        :: FilePath -> m Bool
    CallCommand         :: String   -> FakeIO ()
    Log                 :: String   -> FakeIO ()
    Glob                :: FilePath -> Glob.Pattern -> FakeIO [FilePath]
    GetModificationTime :: FilePath -> FakeIO UTCTime
    Fail                :: String -> FakeIO a

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

instance MonadFail FakeIO where
    fail = Fail


instance AchilleIO FakeIO where
    readFile            = ReadFile
    readFileLazy        = ReadFileLazy
    copyFile            = CopyFile
    writeFile           = WriteFile
    writeFileLazy       = WriteFileLazy
    doesFileExist       = DoesFileExist
    doesDirExist        = DoesDirExist
    callCommand         = CallCommand
    log                 = Log
    glob                = Glob
    getModificationTime = GetModificationTime


data FakeIOActions
    = WrittenFile          FilePath BS.ByteString
    | WrittenFileLazy      FilePath LBS.ByteString
    | HasReadFile          FilePath
    | HasReadFileLazy      FilePath
    | CopiedFile FilePath  FilePath
    | CalledCommand        String
    | Failed               String
    deriving (Eq, Show)


type FileSystem = Map FilePath (UTCTime, BS.ByteString)

defMTime :: UTCTime
defMTime = undefined

getMTime :: FilePath -> FileSystem -> UTCTime
getMTime p fs = fromMaybe defMTime (fst <$> M.lookup p fs)

getBS :: FilePath -> FileSystem -> BS.ByteString
getBS p fs = fromMaybe BS.empty (snd <$> M.lookup p fs)

retrieveFakeIOActions :: FakeIO (a, Cache)    -- the fake IO computation
                      -> FileSystem           -- the underlying input FS
                      -> (Maybe a, [FakeIOActions])
retrieveFakeIOActions t fs = bimap (fmap fst) reverse $ runState (retrieve t) []
    where
        retrieve :: FakeIO a -> State [FakeIOActions] (Maybe a)
        retrieve (ReadFile key) = do
            modify (HasReadFile key :)
            pure (Just $ getBS key fs)

        retrieve (ReadFileLazy key) = do
            modify (HasReadFileLazy key :)
            pure (Just $ LBS.fromStrict $ getBS key fs)

        retrieve (CopyFile from to)  = Just <$> modify (CopiedFile from to :)
        retrieve (WriteFile p c)     = Just <$> modify (WrittenFile p c :)
        retrieve (WriteFileLazy p c) = Just <$> modify (WrittenFileLazy p c :)
        retrieve (DoesFileExist p)   = 

        retrieve (CallCommand cmd)   = Just <$> modify (CalledCommand cmd :)
        retrieve (Log str)           = pure $ Just ()
        retrieve (Glob dir p)        = undefined
        retrieve (GetModificationTime p) = pure $ Just (getMTime p fs)

        retrieve (SeqAp f x) = do
            actions <- get
            let (Just f', actions')  = runState (retrieve f) actions
            let (Just x', actions'') = runState (retrieve x) actions'
            put actions''
            pure $ Just (f' x')

        retrieve (Fmap f x)  = do
            actions <- get
            let (Just x', actions') = runState (retrieve x) actions
            put actions'
            pure $ Just (f x')

        retrieve (Pure x)   = pure (Just x)
        retrieve (Fail str) = modify (Failed str :) >> pure Nothing

        retrieve (Bind (Fail str) f) = modify (Failed str :) >> pure Nothing

        retrieve (Bind x f) = do
            actions <- get
            let (Just x', actions') = runState (retrieve x) actions
            put actions'
            retrieve (f x')


exactRun :: (Show b, Eq b)
    => FileSystem
    -> Context a
    -> Recipe FakeIO a b
-> (Maybe b, [FakeIOActions])
    -> Assertion
    exactRun fs ctx r expected =
    let fakeIO = runRecipe r ctx
    trace  = retrieveFakeIOActions fakeIO fs
    in trace @?= expected
