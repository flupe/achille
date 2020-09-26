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


class (Monad m, MonadFail m) => AchilleIO m where
    readFile            :: FilePath -> m BS.ByteString
    readFileLazy        :: FilePath -> m LBS.ByteString
    copyFile            :: FilePath -> FilePath       -> m ()
    writeFile           :: FilePath -> BS.ByteString  -> m ()
    writeFileLazy       :: FilePath -> LBS.ByteString -> m ()
    doesFileExist       :: FilePath -> m Bool
    doesDirExist        :: FilePath -> m Bool
    callCommand         :: String   -> m ()
    log                 :: String   -> m ()
    glob                :: FilePath -> Glob.Pattern   -> m [FilePath]
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
