-- | Exposes an IO interface used by core achille recipes
module Achille.IO (AchilleIO(..)) where

import Data.Text        (Text)
import Data.Time.Clock  (UTCTime)

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Directory     qualified as Directory
import System.FilePath      qualified as FilePath
import System.FilePath.Glob qualified as Glob
import System.Process       qualified as Process


-- | Interface for IO operations used by core recipes.
class AchilleIO m where
    -- | Retrieve a file as a bytestring.
    readFile :: FilePath -> m BS.ByteString
    -- | Retrieve a file as a /lazy/ bytestring.
    readFileLazy :: FilePath -> m LBS.ByteString
    -- | Copy a file from one location to another.
    copyFile :: FilePath -> FilePath       -> m ()
    -- | Write a bytestring to a file.
    writeFile :: FilePath -> BS.ByteString  -> m ()
    -- | Write a /lazy/ bytestring to a file.
    writeFileLazy :: FilePath -> LBS.ByteString -> m ()
    -- | Check whether a file exists.
    doesFileExist :: FilePath -> m Bool
    -- | Check whether a directory exists.
    doesDirExist :: FilePath -> m Bool
    -- | Run a shell command in a new process.
    callCommand :: String -> m ()
    -- | Run a shell command in a new process.
    readCommand :: String -> [String] -> m String
    -- | Log a string to stdout.
    log :: String -> m ()
    -- | Find all paths matching a given globpattern, relative to a given directory.
    glob :: FilePath -- ^ Path of the root directory.
         -> Glob.Pattern -> m [FilePath]
    -- | Get modification time of a file.
    getModificationTime :: FilePath -> m UTCTime


ensureDirExists :: FilePath -> IO ()
ensureDirExists =
    Directory.createDirectoryIfMissing True . FilePath.takeDirectory


instance AchilleIO IO where
    readFile             = BS.readFile
    readFileLazy         = LBS.readFile
    copyFile from to     = ensureDirExists to *> Directory.copyFile from to
    writeFile to x       = ensureDirExists to *> BS.writeFile to x
    writeFileLazy to x   = ensureDirExists to *> LBS.writeFile to x
    doesFileExist        = Directory.doesFileExist
    doesDirExist         = Directory.doesDirectoryExist
    callCommand          = Process.callCommand
    readCommand cmd args = Process.readProcess cmd args []
    log                  = Prelude.putStrLn
    glob dir pattern     =
      Directory.withCurrentDirectory dir $
          Glob.globDir1 pattern ""
          >>= mapM Directory.makeRelativeToCurrentDirectory
    getModificationTime  = Directory.getModificationTime
