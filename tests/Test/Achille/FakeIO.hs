{-# LANGUAGE GADTs, ScopedTypeVariables, OverloadedStrings, BlockArguments, RecordWildCards #-}

module Test.Achille.FakeIO where

import Data.Bifunctor (bimap, first)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Control.Monad (join)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans (lift)

import Data.Text (Text)
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Directory     qualified as Directory
import System.FilePath      qualified as FilePath
import System.FilePath.Glob qualified as Glob
import Data.Map.Strict      qualified as Map


import Achille.CLI (processDeps)
import Achille.Config
import Achille.DynDeps (DynDeps)
import Achille.Cache (Cache, emptyCache)
import Achille.Diffable (Value(theVal))
import Achille.Path
import Achille.Context (Context(..))
import Achille.Task (Task, runTask)
import Achille.Task.Prim hiding (lastTime, updatedFiles, outputDir)
import Achille.IO hiding ()

import Test.Tasty.HUnit
import Test.Achille.Common

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


-- TODO(flupe): actually update the FS?
type FakeIO = ReaderT FileSystem (WriterT [IOActions] IO)


instance AchilleIO FakeIO where
    readFile key = asks (getBS key) <* tell [HasReadFile key]
    readFileLazy key =  asks (LBS.fromStrict . getBS key) <* tell [HasReadFileLazy key]
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
    glob r pat = asks (globFS r pat)


runFakeIO :: FakeIO a -> FileSystem -> IO (a, [IOActions])
runFakeIO c fs = runWriterT (runReaderT c fs)


data TestState = TState
  { tCache       :: Maybe Cache
  , tDeps        :: DynDeps
  , tFS          :: FileSystem
  , tLastTime    :: UTCTime
  , tCurrentTime :: UTCTime
  }

defaultState = TState
  { tCache       = Nothing
  , tDeps        = mempty
  , tFS          = baseFS
  , tLastTime    = defMTime
  , tCurrentTime = defMTime
  }


-- | Monad to define consecutive runs with changing inputs on the filesystem
type TestRun a =
  ReaderT (Task FakeIO a)
    (StateT TestState IO)

testRun
  :: Task FakeIO a
  -> TestRun a ()
  -> IO ()
testRun t tr = evalStateT (runReaderT tr t) defaultState

waitASec :: TestRun a ()
waitASec = modify \s ->
  s {tCurrentTime = addUTCTime 1 $ tCurrentTime s}

buildAndExpect
  :: (HasCallStack, Show a, Eq a)
  => (Maybe a, [IOActions])
  -> TestRun a ()
buildAndExpect (eval, eactions) = ReaderT \t -> StateT \TState{..} -> do
  (updates, preactions) <- runFakeIO (processDeps baseCfg tDeps) tFS
  let ctx = baseCtx
        { updatedFiles = updates
        , lastTime     = tLastTime
        , cleanBuild   = isNothing tCache
        }
  ((res, cache, deps), actions) <-
    runFakeIO (runTask t ctx (fromMaybe emptyCache tCache)) tFS

  -- expectations
  theVal <$> res          @?= eval
  (preactions ++ actions) @?= eactions

  -- print deps
  -- updated state
  pure ((), TState (Just cache) deps tFS tCurrentTime tCurrentTime)


exactRun
  :: (Show b, Eq b)
  => Task FakeIO b
  -> (Maybe b, [IOActions])
  -> Assertion
exactRun t = testRun t . buildAndExpect
