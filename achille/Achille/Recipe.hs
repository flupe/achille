{-# LANGUAGE GADTs, ApplicativeDo, RecordWildCards #-}
module Achille.Recipe
  ( module Achille.Core.Recipe
  , readText
  , readByteString
  , debug
  , write
  , copy
  , -- * Recipes over lists
    reverse
  , sort
  , sortOn
  , take
  , drop
  ) where

import Prelude hiding (reverse, take, drop)
import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.List qualified as List (sortOn)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import System.FilePath ((</>))

import Prelude qualified

import Achille.IO as AIO
import Achille.Diffable
import Achille.Core.Recipe (Context(..), Recipe, PrimRecipe, recipe, runRecipe)
import Achille.Writable (Writable)
import Achille.Writable qualified as Writable


-- | Read text from file.
readText :: (Applicative m, AchilleIO m) => Recipe m FilePath Text
readText = recipe "readText" \Context{..} cache v@(src, _) -> do
  let path = inputRoot </> src
  time <- getModificationTime path
  text <- decodeUtf8 <$> AIO.readFile path
  pure (value text (time > lastTime || hasChanged v), cache)

-- | Read a bytestring from file.
readByteString :: (Applicative m, AchilleIO m) => Recipe m FilePath ByteString
readByteString = recipe "readText" \Context{..} cache v@(src, _) -> do
  let path = inputRoot </> src
  time <- getModificationTime path
  text <- AIO.readFile path
  pure (value text (time > lastTime || hasChanged v), cache)

-- | Print a message to stdout.
debug :: (Applicative m, AchilleIO m) => Recipe m Text ()
debug = recipe "debug" \ctx cache (msg, _) -> AIO.log (unpack msg) *> pure (unit, cache)

-- | Write something to file, /iff/ this thing has changed since the last run.
write
  :: (Applicative m, AchilleIO m, Writable m a)
  => Recipe m (FilePath, a) FilePath
write = recipe "write" \Context{..} cache v@((src, x), _) -> do
  let path = outputRoot </> sitePrefix </> src
  let vsrc = fst (splitPair v)
  -- NOTE(flupe): maybe also when the output file is no longer here
  when (hasChanged v) $ AIO.log ("Writing " <> path) *> Writable.write path x
  pure (value ("/" <> sitePrefix </> src) (hasChanged v), cache)

-- | Copies a file to the output path, preserving its name.
copy :: (Monad m, AchilleIO m) => Recipe m FilePath FilePath
copy = recipe "copy" \Context{..} cache v@(src, _) -> do
  let ipath = inputRoot </> src
  let opath = outputRoot </> sitePrefix </> src
  -- TODO(flupe): check if file exists
  -- TODO(flupe): check if output file is there?
  time <- getModificationTime ipath
  when (hasChanged v || time > lastTime) $
    AIO.log ("Copying " <> ipath) *> AIO.copyFile ipath opath
  pure (value ("/" <> sitePrefix </> src) (hasChanged v), cache)

-- | Reverse a list.
reverse :: Applicative m => Recipe m [a] [a]
reverse = recipe "reverse" \_ cache v -> pure (joinList (Prelude.reverse (splitList v)), cache)

-- | Sort a list using the prelude @sort@.
--   Crucially this takes care of tracking change information in the list.
sort :: (Applicative m, Ord a) => Recipe m [a] [a]
sort = recipe "sort" \_ cache v -> pure (joinList (List.sortOn fst (splitList v)), cache)

-- | Sort a list using the prelude @sort@.
--   Crucially this takes care of tracking change information in the list.
sortOn :: (Applicative m, Ord b) => (a -> b) -> Recipe m [a] [a]
sortOn f = recipe "sortOn" \_ cache v -> pure (joinList (List.sortOn (f . fst) (splitList v)), cache)

-- TODO(flupe): make Int argument a task
-- NOTE: not optimal, take/drop both lists?
-- | Return the prefix of length @n@ of the input list.
take :: (Applicative m) => Int -> Recipe m [a] [a]
take n = recipe "take" \_ cache v -> pure (joinList (Prelude.take n (splitList v)), cache)

-- | Drop the first @n@ elements of the input list.
drop :: Applicative m => Int -> Recipe m [a] [a]
drop n = recipe "drop" \_ cache v -> pure (joinList (Prelude.drop n (splitList v)), cache)
