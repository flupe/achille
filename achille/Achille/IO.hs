-- | Exposes an IO interface used by core achille recipes
module Achille.IO (AchilleIO(..)) where

import Data.Text        (Text)
import Data.Time.Clock  (UTCTime)
import Data.String (fromString)
import System.IO as SIO

import Data.Time qualified as Time (getCurrentTime)
import Data.ByteString        qualified as BS
import Data.ByteString.Lazy   qualified as LBS
import Data.Text.IO           qualified as Text
import System.Directory       qualified as Directory
import System.FilePath        qualified as FilePath
import System.FilePath.Glob   qualified as Glob
import System.Process         qualified as Process

import Achille.Path


-- | Interface for IO operations used by core recipes.
class AchilleIO m where
    -- | Retrieve a file as a bytestring.
    readFile :: Path -> m BS.ByteString
    -- | Retrieve a file as a /lazy/ bytestring.
    readFileLazy :: Path -> m LBS.ByteString
    -- | Copy a file from one location to another.
    copyFile :: Path -> Path       -> m ()
    -- | Write a bytestring to a file.
    writeFile :: Path -> BS.ByteString  -> m ()
    -- | Write a /lazy/ bytestring to a file.
    writeFileLazy :: Path -> LBS.ByteString -> m ()
    -- | Check whether a file exists.
    doesFileExist :: Path -> m Bool
    -- | Check whether a directory exists.
    doesDirExist :: Path -> m Bool
    -- | Return all entries in directory, without special entries like @.@ and @..@.
    listDir :: Path -> m [Path]
    -- | Run a shell command in a new process.
    callCommand :: String -> m ()
    -- | Run a shell command in a new process.
    readCommand :: String -> [String] -> m String
    -- | Log some text to stdout, followed by a newline.
    log :: Text -> m ()

    -- | Find all paths matching a given globpattern, relative to a given directory.
    --   All paths returned must be relative to the current working directory.
    glob :: Path -- ^ Path of the prefix directory
         -> Glob.Pattern -> m [Path]
    -- TODO(flupe): ^ really think about current working dir, absolute paths and prefixes
    -- | Get modification time of a file.
    getModificationTime :: Path -> m UTCTime

    getCurrentTime :: m UTCTime


ensureDirExists :: Path -> IO ()
ensureDirExists = Directory.createDirectoryIfMissing True . toFilePath . takeDirectory


instance AchilleIO IO where
    readFile             = BS.readFile . toFilePath
    readFileLazy         = LBS.readFile . toFilePath
    copyFile from to     = ensureDirExists to *> Directory.copyFile (toFilePath from) (toFilePath to)
    writeFile to x       = ensureDirExists to *> BS.writeFile (toFilePath to) x
    writeFileLazy to x   = ensureDirExists to *> LBS.writeFile (toFilePath to) x
    doesFileExist        = Directory.doesFileExist . toFilePath
    doesDirExist         = Directory.doesDirectoryExist . toFilePath
    listDir              = fmap (map fromString) . Directory.listDirectory . toFilePath
    callCommand          = Process.callCommand
    readCommand cmd args = Process.readProcess cmd args []
    log                  = Text.putStrLn
    glob dir pattern     = map fromString <$> Glob.globDir1 pattern (toFilePath dir)
    getModificationTime  = Directory.getModificationTime . toFilePath
    getCurrentTime       = Time.getCurrentTime
