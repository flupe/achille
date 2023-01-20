{-# LANGUAGE GADTs, ScopedTypeVariables, OverloadedStrings #-}

module Test.Achille.FakeIO where

import Data.Bifunctor (bimap, first)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Time.Clock (UTCTime(..))
import Control.Monad (join)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.Writer

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Directory     qualified as Directory
import System.FilePath      qualified as FilePath
import System.FilePath.Glob qualified as Glob
import Data.Map.Strict      qualified as Map

import Test.Tasty.HUnit

import Achille.Cache (Cache, emptyCache)
import Achille.Diffable (Value(theVal))
import Achille.Path
import Achille.Context (Context)
import Achille.Task (Task, runTask)
import Achille.Task.Prim
import Achille.IO hiding ()

-- | Poor man's glob. Inefficient, but that's not the point.
globFS :: Path -> Glob.Pattern -> FileSystem -> [Path]
globFS root pat fs =
  let files = map (makeRelative "content") $ Map.keys fs
  in filter (\src -> Glob.matchWith Glob.matchDefault pat (toFilePath src)) files

data FakeIO a where
    ReadFile            :: Path -> FakeIO BS.ByteString
    ReadFileLazy        :: Path -> FakeIO LBS.ByteString
    CopyFile            :: Path -> Path -> FakeIO ()
    WriteFile           :: Path -> BS.ByteString  -> FakeIO ()
    WriteFileLazy       :: Path -> LBS.ByteString -> FakeIO ()
    DoesFileExist       :: Path -> FakeIO Bool
    DoesDirExist        :: Path -> FakeIO Bool
    CallCommand         :: String -> FakeIO ()
    Glob                :: Path -> Glob.Pattern -> FakeIO [Path]
    GetModificationTime :: Path -> FakeIO UTCTime
    Fail                :: String -> FakeIO a

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
    = WrittenFile Path BS.ByteString
    | WrittenFileLazy Path LBS.ByteString
    | HasReadFile Path
    | HasReadFileLazy Path
    | CheckedFile Path
    | CheckedMTime Path
    | CopiedFile Path Path
    | CalledCommand String
    | Failed String
    deriving (Eq, Show)

data File = File
  { mtime    :: UTCTime
  , contents :: BS.ByteString
  }

type FileSystem = Map Path File

defMTime :: UTCTime
defMTime = UTCTime (toEnum 0) 0

getMTime :: Path -> FileSystem -> UTCTime
getMTime src = maybe defMTime mtime . Map.lookup src

getBS :: Path -> FileSystem -> BS.ByteString
getBS src = maybe BS.empty contents . Map.lookup src

retrieveActions
  :: FakeIO a    -- the fake IO computation
  -> FileSystem  -- the underlying input FS
  -> (Maybe a, [Actions])
retrieveActions t fs = runWriter (retrieve t)
  where
    retrieve :: FakeIO a -> Writer [Actions] (Maybe a)
    retrieve (ReadFile key)     = writer (Just (getBS key fs), [HasReadFile key])
    retrieve (ReadFileLazy key) = writer (Just (LBS.fromStrict $ getBS key fs), [HasReadFileLazy key])

    retrieve (CopyFile from to)  = Just <$> tell [CopiedFile from to]
    retrieve (WriteFile p c)     = Just <$> tell [WrittenFile p c]
    retrieve (WriteFileLazy p c) = Just <$> tell [WrittenFileLazy p c]
    retrieve (DoesFileExist p)   = writer (Just $ Map.member p fs, [CheckedFile p])

    retrieve (CallCommand cmd)   = Just <$> tell [CalledCommand cmd]
    retrieve (Glob dir p)        = pure $ Just $ globFS dir p fs -- undefined
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
    let fakeIO = runTask t ctx emptyCache <&> \(v, x, y) -> v
        trace = retrieveActions fakeIO fs
    in (first (fmap theVal . join) trace @?=)
