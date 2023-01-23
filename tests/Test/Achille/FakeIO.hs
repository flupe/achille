{-# LANGUAGE GADTs, ScopedTypeVariables, OverloadedStrings #-}

module Test.Achille.FakeIO where

import Data.Bifunctor (bimap, first)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Time.Clock (UTCTime(..))
import Control.Monad (join)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader

import Data.Text (Text)
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


data File = File
  { mtime    :: UTCTime
  , contents :: BS.ByteString
  }

type FileSystem = Map Path File

defMTime :: UTCTime
defMTime = UTCTime (toEnum 0) 0

getMTime :: Path -> FakeIO UTCTime
getMTime src = asks (maybe defMTime mtime . Map.lookup src)

getBS :: Path -> FakeIO BS.ByteString
getBS src = asks (maybe BS.empty contents . Map.lookup src)

-- | Poor man's glob. Inefficient, but that's not the point.
globFS :: Path -> Glob.Pattern -> FakeIO [Path]
globFS root pat = do
  fs <- ask
  let files = map (makeRelative "content") $ Map.keys fs
  pure $ filter (\src -> Glob.matchWith Glob.matchDefault pat (toFilePath src)) files

data IOActions
    = WrittenFile Path BS.ByteString
    | WrittenFileLazy Path LBS.ByteString
    | HasReadFile Path
    | HasReadFileLazy Path
    | CheckedFile Path
    | CheckedMTime Path
    | CopiedFile Path Path
    | CalledCommand String
    | Failed String
    | Logged Text
    deriving (Eq, Show)

type FakeIO = ReaderT FileSystem (WriterT [IOActions] IO)

instance AchilleIO FakeIO where
    readFile key = getBS key <* tell [HasReadFile key]
    readFileLazy key = LBS.fromStrict <$> getBS key <* tell [HasReadFileLazy key]
    copyFile from to = tell [CopiedFile from to]
    writeFile p c = tell [WrittenFile p c]
    writeFileLazy p c = tell [WrittenFileLazy p c]
    doesFileExist p = asks (Map.member p) <* tell [CheckedFile p]
    callCommand cmd = tell [CalledCommand cmd]
    getModificationTime src = tell [CheckedMTime src] *> asks (maybe defMTime mtime . Map.lookup src)
    doesDirExist = undefined
    listDir = undefined
    readCommand = undefined
    log s = tell [Logged s]
    glob = globFS

runFakeIO :: FakeIO a -> FileSystem -> IO (a, [IOActions])
runFakeIO c fs = runWriterT (runReaderT c fs)

exactRun :: (Show b, Eq b)
  => FileSystem
  -> Context
  -> Task FakeIO b
  -> (Maybe b, [IOActions])
  -> Assertion
exactRun fs ctx t expected = do
    let fakeIO = runTask t ctx emptyCache <&> \(v, x, y) -> v
    trace <- runFakeIO fakeIO fs
    first (fmap theVal) trace @?= expected
