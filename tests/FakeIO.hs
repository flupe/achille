{-# LANGUAGE GADTs, ScopedTypeVariables #-}

module FakeIO where

import Data.Bifunctor (bimap)
import Data.Map.Strict as Map
import Data.Maybe
import Data.Time.Clock (UTCTime(..))
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.Writer

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Directory     qualified as Directory
import System.FilePath      qualified as FilePath
import System.FilePath.Glob qualified as Glob

import Test.Tasty.HUnit

import Achille.Cache (Cache, emptyCache)
import Achille.Diffable (Value(theVal))
import Achille.Recipe (Context, Result(output))
import Achille.Task (Task, runTask)
import Achille.IO

data FakeIO a where
    ReadFile            :: FilePath -> FakeIO BS.ByteString
    ReadFileLazy        :: FilePath -> FakeIO LBS.ByteString
    CopyFile            :: FilePath -> FilePath       -> FakeIO ()
    WriteFile           :: FilePath -> BS.ByteString  -> FakeIO ()
    WriteFileLazy       :: FilePath -> LBS.ByteString -> FakeIO ()
    DoesFileExist       :: FilePath -> FakeIO Bool
    DoesDirExist        :: FilePath -> FakeIO Bool
    CallCommand         :: String   -> FakeIO ()
    Glob                :: FilePath -> Glob.Pattern -> FakeIO [FilePath]
    GetModificationTime :: FilePath -> FakeIO UTCTime
    Fail                :: String   -> FakeIO a

    SeqAp               :: FakeIO (a -> b) -> FakeIO a -> FakeIO b
    Fmap                :: (a -> b) -> FakeIO a -> FakeIO b
    Pure                :: a -> FakeIO a
    Bind                :: FakeIO a -> (a -> FakeIO b) -> FakeIO b

instance Functor FakeIO where fmap = Fmap
instance Applicative FakeIO where pure = Pure; (<*>) = SeqAp
instance Monad FakeIO where (>>=) = Bind
instance MonadFail FakeIO where fail = Fail

instance AchilleIO FakeIO where
    readFile            = ReadFile
    readFileLazy        = ReadFileLazy
    copyFile            = CopyFile
    writeFile           = WriteFile
    writeFileLazy       = WriteFileLazy
    doesFileExist       = DoesFileExist
    doesDirExist        = DoesDirExist
    callCommand         = CallCommand
    log s               = pure ()
    glob                = Glob
    listDir             = undefined
    readCommand         = undefined
    getModificationTime = GetModificationTime


data Actions
    = WrittenFile         FilePath BS.ByteString
    | WrittenFileLazy     FilePath LBS.ByteString
    | HasReadFile         FilePath
    | HasReadFileLazy     FilePath
    | CheckedFile         FilePath
    | CheckedMTime        FilePath
    | CopiedFile FilePath FilePath
    | CalledCommand       String
    | Failed              String
    deriving (Eq, Show)

data File = File
  { mtime    :: UTCTime
  , contents :: BS.ByteString
  }

type FileSystem = Map FilePath File

defMTime :: UTCTime
defMTime = UTCTime (toEnum 0) 0

getMTime :: FilePath -> FileSystem -> UTCTime
getMTime src = maybe defMTime mtime . Map.lookup src

getBS :: FilePath -> FileSystem -> BS.ByteString
getBS src = maybe BS.empty contents . Map.lookup src

retrieveActions
  :: FakeIO (Result a)    -- the fake IO computation
  -> FileSystem           -- the underlying input FS
  -> (Maybe a, [Actions])
retrieveActions t fs = runWriter (retrieve (fmap (theVal . output) t))
  where
    retrieve :: FakeIO a -> Writer [Actions] (Maybe a)
    retrieve (ReadFile key)     = writer (Just (getBS key fs), [HasReadFile key])
    retrieve (ReadFileLazy key) = writer (Just (LBS.fromStrict $ getBS key fs), [HasReadFileLazy key])

    retrieve (CopyFile from to)  = Just <$> tell [CopiedFile from to]
    retrieve (WriteFile p c)     = Just <$> tell [WrittenFile p c]
    retrieve (WriteFileLazy p c) = Just <$> tell [WrittenFileLazy p c]
    retrieve (DoesFileExist p)   = writer (Just $ Map.member p fs, [CheckedFile p])

    retrieve (CallCommand cmd)   = Just <$> tell [CalledCommand cmd]
    retrieve (Glob dir p)        = undefined -- undefined
    retrieve (GetModificationTime p) = writer (Just (getMTime p fs), [CheckedMTime p])

    retrieve (SeqAp f x) = do
      f' <- retrieve f
      x' <- retrieve x
      pure (f' <*> x')

    retrieve (Fmap f x) = do
      x' <- retrieve x
      pure (f <$> x')

    retrieve (Pure x)   = pure (Just x)
    retrieve (Fail str) = writer (Nothing, [Failed str])

    retrieve (Bind x f) = do
      x' <- retrieve x
      case x' of
        Nothing -> pure Nothing
        Just x' -> retrieve (f x')

exactRun :: (Show b, Eq b)
  => FileSystem
  -> Context
  -> Task FakeIO b
  -> (Maybe b, [Actions])
  -> Assertion
exactRun fs ctx t =
    let fakeIO = runTask t ctx emptyCache
        trace = retrieveActions fakeIO fs
    in (trace @?=)
