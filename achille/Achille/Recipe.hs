{-# LANGUAGE GADTs, ApplicativeDo, RecordWildCards #-}
module Achille.Recipe
  ( module Achille.Core.Recipe
  , readText
  , readByteString
  , debug
  , write
  , copy
  , -- * Recipes over lists
    map
  , reverse
  , sort
  , sortOn
  , take
  , drop
  , (!)
  ) where

import Prelude hiding (reverse, take, drop, map)
import Control.Monad (when)
import Data.Bifunctor (bimap, first)
import Data.List qualified as List (sortOn)
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import System.FilePath ((</>))

import Prelude qualified
import Data.Map.Strict qualified as Map

import Achille.IO as AIO
import Achille.Diffable
import Achille.Core.Recipe (Context(..), Recipe, PrimRecipe, recipe, runRecipe)
import Achille.Writable (Writable)
import Achille.Writable qualified as Writable


-- | Read text from file.
readText :: (Applicative m, AchilleIO m) => Recipe m FilePath Text
readText = recipe "readText" \Context{..} cache v -> do
  let path = inputRoot </> theVal v
  time <- getModificationTime path
  text <- decodeUtf8 <$> AIO.readFile path
  pure (value (time > lastTime || hasChanged v) text, cache)

-- | Read a bytestring from file.
readByteString :: (Applicative m, AchilleIO m) => Recipe m FilePath ByteString
readByteString = recipe "readText" \Context{..} cache v -> do
  let path = inputRoot </> theVal v
  time <- getModificationTime path
  text <- AIO.readFile path
  pure (value (time > lastTime || hasChanged v) text, cache)

-- | Print a message to stdout.
debug :: (Applicative m, AchilleIO m) => Recipe m Text ()
debug = recipe "debug" \ctx cache v -> AIO.log (unpack $ theVal v) *> pure (unit, cache)

-- | Write something to file, /iff/ this thing has changed since the last run.
write
  :: (Applicative m, AchilleIO m, Writable m a)
  => Recipe m (FilePath, a) FilePath
write = recipe "write" \Context{..} cache v -> do
  let (vsrc, vx) = splitValue v
  let path = outputRoot </> sitePrefix </> theVal vsrc
  -- NOTE(flupe): maybe also when the output file is no longer here
  when (hasChanged v) $ AIO.log ("Writing " <> path) *> Writable.write path (theVal vx)
  pure (value (hasChanged v) ("/" <> sitePrefix </> theVal vsrc) , cache)

-- | Copies a file to the output path, preserving its name.
copy :: (Monad m, AchilleIO m) => Recipe m FilePath FilePath
copy = recipe "copy" \Context{..} cache vsrc -> do
  let ipath = inputRoot </> theVal vsrc
  let opath = outputRoot </> sitePrefix </> theVal vsrc
  -- TODO(flupe): check if file exists
  -- TODO(flupe): check if output file is there?
  time <- getModificationTime ipath
  when (hasChanged vsrc || time > lastTime) $
    AIO.log ("Copying " <> ipath) *> AIO.copyFile ipath opath
  pure (value (hasChanged vsrc) ("/" <> sitePrefix </> theVal vsrc), cache)

-- | Map a function over a list.
map :: Applicative m => (a -> b) -> Recipe m [a] [b]
map f = recipe "map" \_ cache v -> pure . (, cache) $
  case v of
    Value xs c Nothing   -> value c (Prelude.map f xs)
    Value xs c (Just vs) -> Value (Prelude.map f xs) c (Just (fmap f <$> vs))

-- | Reverse a list.
reverse :: Applicative m => Recipe m [a] [a]
reverse = recipe "reverse" \_ cache v -> pure (joinValue (Prelude.reverse (splitValue v)), cache)

-- | Sort a list using the prelude @sort@.
--   Crucially this takes care of tracking change information in the list.
sort :: (Applicative m, Ord a) => Recipe m [a] [a]
sort = recipe "sort" \_ cache v -> pure (joinValue (List.sortOn theVal (splitValue v)), cache)

-- | Sort a list using the prelude @sort@.
--   Crucially this takes care of tracking change information in the list.
sortOn :: (Applicative m, Ord b) => (a -> b) -> Recipe m [a] [a]
sortOn f = recipe "sortOn" \_ cache v -> pure (joinValue (List.sortOn (f . theVal) (splitValue v)), cache)

-- TODO(flupe): make Int argument a task
-- NOTE: not optimal, take/drop both lists?
-- | Return the prefix of length @n@ of the input list.
take :: (Applicative m) => Int -> Recipe m [a] [a]
take n = recipe "take" \_ cache v -> pure (joinValue (Prelude.take n (splitValue v)), cache)

-- | Drop the first @n@ elements of the input list.
drop :: Applicative m => Int -> Recipe m [a] [a]
drop n = recipe "drop" \_ cache v -> pure (joinValue (Prelude.drop n (splitValue v)), cache)

-----------

(!) :: (Applicative m, Ord k) => Recipe m (Map k v, k) v
(!) = recipe "(!)" \_ cache v -> do
  let (vm, vk) = splitValue v
  let vs = splitValue vm
  let vv = if hasChanged vk then value True (theVal vm Map.! theVal vk)
                            else vs Map.! theVal vk
  pure (vv, cache)
