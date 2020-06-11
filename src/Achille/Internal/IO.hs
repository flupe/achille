{-# LANGUAGE GADTs #-}

module Achille.Internal.IO
    ( AchilleIO
    , readFileText
    , readFile
    , readFileLazy
    , copyFile
    , writeFile
    , callCommand
    , log
    , FakeIO(..)
    ) where

import Prelude as Prelude hiding (log, readFile, writeFile)

import Data.Text       (Text)
import System.FilePath (FilePath)

import qualified System.FilePath      as FilePath
import qualified System.Directory     as Directory
import qualified System.Process       as Process
import qualified Data.Text            as Text
import qualified Data.Text.IO         as TextIO
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy as LBS


import Achille.Writable     as Writable


class Monad m => AchilleIO m where
    readFileText :: FilePath -> m Text
    readFile     :: FilePath -> m BS.ByteString
    readFileLazy :: FilePath -> m LBS.ByteString
    copyFile     :: FilePath -> FilePath -> m ()
    writeFile    :: Writable a => FilePath -> a -> m ()
    callCommand  :: String -> m ()
    log          :: String -> m ()


ensureDirExists :: FilePath -> IO ()
ensureDirExists =
    Directory.createDirectoryIfMissing True . FilePath.takeDirectory

instance AchilleIO IO where
    readFileText     = TextIO.readFile
    readFile         = BS.readFile
    readFileLazy     = LBS.readFile
    copyFile from to = ensureDirExists to >> Directory.copyFile from to
    writeFile to x   = ensureDirExists to >> Writable.write to x
    callCommand      = Process.callCommand
    log              = Prelude.putStrLn


data FakeIO a where
    ReadFileText :: FilePath -> FakeIO Text
    ReadFile     :: FilePath -> FakeIO BS.ByteString
    ReadFileLazy :: FilePath -> FakeIO LBS.ByteString
    CopyFile     :: FilePath -> FilePath -> FakeIO ()
    WriteFile    :: Writable a => FilePath -> a -> FakeIO ()
    CallCommand  :: String -> FakeIO ()
    Log          :: String -> FakeIO ()

    SeqAp        :: FakeIO (a -> b) -> FakeIO a -> FakeIO b
    Fmap         :: (a -> b) -> FakeIO a -> FakeIO b
    Pure         :: a -> FakeIO a
    Bind         :: FakeIO a -> (a -> FakeIO b) -> FakeIO b

instance Functor FakeIO where
    fmap = Fmap

instance Applicative FakeIO where
    pure  = Pure
    (<*>) = SeqAp

instance Monad FakeIO where
    (>>=) = Bind

instance AchilleIO FakeIO where
    readFileText = ReadFileText
    readFile     = ReadFile
    readFileLazy = ReadFileLazy
    copyFile     = CopyFile
    writeFile    = WriteFile
    callCommand  = CallCommand
    log          = Log

data FakeIOActions
    = WrittenFile  FilePath
    | HasReadFileText FilePath
    | HasReadFile     FilePath
    | HasReadFileLazy FilePath
    | CopiedFile FilePath FilePath
    | CalledCommand String
    | Logged String
    deriving (Eq, Show)
