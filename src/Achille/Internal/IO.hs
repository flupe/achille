-- | Defines an IO interface for core recipes
module Achille.Internal.IO
    ( AchilleIO
    , readFile
    , readFileLazy
    , copyFile
    , writeFile
    , writeFileLazy
    , doesFileExist
    , doesDirExist
    , callCommand
    , log
    , glob
    , getModificationTime
    , fail
    ) where

import Prelude as Prelude hiding (log, readFile, writeFile)

import Data.Text          (Text)
import System.FilePath    (FilePath)
import Data.Time.Clock    (UTCTime)
import Control.Monad.Fail (MonadFail)

import qualified System.Directory     as Directory
import qualified System.FilePath      as FilePath
import qualified System.FilePath.Glob as Glob
import qualified System.Process       as Process
import qualified Data.Text            as Text
import qualified Data.Text.IO         as TextIO
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS


-- | Interface for IO actions used by core recipes.
class (Monad m, MonadFail m) => AchilleIO m where
    -- | Retrieve a file as a bytestring.
    readFile            :: FilePath -> m BS.ByteString
    -- | Retrieve a file as a /lazy/ bytestring.
    readFileLazy        :: FilePath -> m LBS.ByteString
    -- | Copy a file from one location to another.
    copyFile            :: FilePath -> FilePath       -> m ()
    -- | Write a bytestring to a file.
    writeFile           :: FilePath -> BS.ByteString  -> m ()
    -- | Write a /lazy/ bytestring to a file.
    writeFileLazy       :: FilePath -> LBS.ByteString -> m ()
    -- | Check whether a file exists.
    doesFileExist       :: FilePath -> m Bool
    -- | Check whether a directory exists.
    doesDirExist        :: FilePath -> m Bool
    -- | Run a shell command in a new process.
    callCommand         :: String   -> m ()
    -- | Log a string to stdout.
    log                 :: String   -> m ()
    -- | Find all paths matching a given globpattern, relative to a given directory.
    glob                :: FilePath -- ^ Path of the root directory.
                        -> Glob.Pattern   -> m [FilePath]
    -- | Get modification time of a file.
    getModificationTime :: FilePath -> m UTCTime


ensureDirExists :: FilePath -> IO ()
ensureDirExists =
    Directory.createDirectoryIfMissing True . FilePath.takeDirectory


instance AchilleIO IO where
    readFile            = BS.readFile
    readFileLazy        = LBS.readFile
    copyFile from to    = ensureDirExists to >> Directory.copyFile from to
    writeFile to x      = ensureDirExists to >> BS.writeFile to x
    writeFileLazy to x  = ensureDirExists to >> LBS.writeFile to x
    doesFileExist       = Directory.doesFileExist
    doesDirExist        = Directory.doesDirectoryExist
    callCommand         = Process.callCommand
    log                 = Prelude.putStrLn
    glob dir pattern    =
        Directory.withCurrentDirectory dir $
            Glob.globDir1 pattern ""
            >>= mapM Directory.makeRelativeToCurrentDirectory
    getModificationTime = Directory.getModificationTime
