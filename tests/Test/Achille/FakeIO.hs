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

import Test.Tasty.HUnit

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


data File = File
  { mtime    :: UTCTime
  , contents :: BS.ByteString
  }

type FileSystem = Map Path File

-- | Base FS used for tests.
baseFS :: FileSystem
baseFS = Map.fromList
  [ ("content" </> "fichier.txt",         File defMTime "helloworld")
  , ("content" </> "post.md",             File defMTime "<em>hello</em>")
  , ("content" </> "other-post.md",       File defMTime "<strong>hello</strong>")
  , ("content" </> "dir1" </> "index.md", File defMTime "somecontent")
  , ("content" </> "dir1" </> "meta.md",  File defMTime "metadata of dir1")
  , ("content" </> "dir2" </> "index.md", File defMTime "some content again")
  , ("content" </> "dir2" </> "meta.md",  File defMTime "metadata of dir2")
  ]

baseCfg :: Config
baseCfg = defaultConfig { outputDir = "output" }

-- | Base context used to run tasks.
baseCtx :: Context
baseCtx = Context
  { lastTime     = defMTime
  , updatedFiles = Map.empty
  , currentDir   = ""
  , cleanBuild   = True
  , siteConfig   = baseCfg
  , verbose      = False
  , colorful     = False
  }

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


data TestState = TState
  { tCache       :: Cache
  , tDeps        :: DynDeps
  , tFS          :: FileSystem
  , tLastTime    :: UTCTime
  , tCurrentTime :: UTCTime
  }

defaultState = TState
  { tCache       = emptyCache
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
  let ctx = baseCtx { updatedFiles = updates
                    , lastTime = tLastTime
                    , cleanBuild = False
                    }
  ((res, cache, deps), actions) <- runFakeIO (runTask t ctx tCache) tFS

  -- expectations
  theVal <$> res @?= eval
  (preactions ++ actions) @?= eactions

  -- updated state
  pure ((), TState cache deps tFS tCurrentTime tCurrentTime)
