{-# LANGUAGE GADTs, ApplicativeDo, OverloadedStrings #-}
module Achille.Recipe
  ( module Achille.Core.Recipe
  , log
  , debug
  , readText
  , readByteString
  , toURL
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

import Prelude hiding (reverse, take, drop, map, log, readFile)
import Control.Monad (when)
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Arrow
import Data.Functor (($>))
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)

import Achille.IO hiding (log)
import Achille.Diffable
import Achille.Core.Recipe ( Recipe, PrimRecipe , recipe, runRecipe)
import Achille.DynDeps
import Achille.Path
import Achille.Task.Prim
import Achille.Writable (Writable)

import Prelude           qualified
import Data.List         qualified as List (sortOn)
import Data.Map.Strict   qualified as Map
import Achille.IO        qualified as AIO
import Achille.Writable  qualified as Writable
import Achille.Context   qualified as Ctx
import Achille.Config    qualified as Cfg

-- TODO(flupe): remove getModificationTime invocations when needed

-- | Read a bytestring from file.
readByteString :: (Monad m, AchilleIO m) => Recipe m Path ByteString
readByteString = recipe "readText" \v -> do
  path     <- (</>) <$> contentDir <*> pure (theVal v)
  lastTime <- lastTime
  time <- getModificationTime path
  logInfo ("Reading " <> pack (toFilePath path))
  text <- readFile path
  pure (value (time > lastTime || hasChanged v) text)

-- | Read text from file.
readText :: (Monad m, AchilleIO m) => Recipe m Path Text
readText = recipe "readText" \v -> do
  path     <- (</>) <$> contentDir <*> pure (theVal v)
  lastTime <- lastTime
  time <- getModificationTime path
  logInfo ("Reading " <> pack (toFilePath path))
  text <- decodeUtf8 <$> readFile path
  pure (value (time > lastTime || hasChanged v) text)

-- | Print a message to stdout.
log :: (Monad m, AchilleIO m) => Recipe m Text ()
log = recipe "log" \v -> AIO.log (theVal v) $> unit

-- | Print a /debug/ message to stdout.
debug :: (Monad m, AchilleIO m) => Recipe m Text ()
debug = recipe "debug" \v -> logDebug (theVal v) $> unit

-- | Convert a path to a proper absolute URL, including the site prefix.
toURL :: Monad m => Recipe m Path Text
toURL = recipe "Achille.Recipe.toURL" \v -> do
  sitePrefix <- reader (Cfg.sitePrefix . Ctx.siteConfig)
  pure $ value (hasChanged v) ("/" <> sitePrefix <> pack (toFilePath (theVal v)))

-- | Write something to file, /iff/ this thing has changed since the last run.
write
  :: (Monad m, AchilleIO m, Writable m a)
  => Recipe m (Path, a) Text
write = toURL <<< recipe "write" \v -> do
  let (vsrc, vx) = splitValue v
  path       <- (</>) <$> outputDir <*> pure (theVal vsrc)
  cleanBuild <- reader Ctx.cleanBuild
  -- NOTE(flupe): maybe also when the output file is no longer here
  when (cleanBuild || hasChanged v) do
    logInfo ("Writing " <> pack (toFilePath path))
    lift (Writable.write path (theVal vx))
  pure vsrc

-- | Copies a file to the output path, preserving its name.
copy :: (Monad m, AchilleIO m) => Recipe m Path Text
copy = toURL <<< recipe "copy" \vsrc -> do
  ipath <- (</>) <$> contentDir <*> pure (theVal vsrc)
  opath <- (</>) <$> outputDir  <*> pure (theVal vsrc)
  lastTime <- lastTime
  cleanBuild <- reader Ctx.cleanBuild
  -- TODO(flupe): check if file exists
  -- TODO(flupe): check if output file is there?
  time <- AIO.getModificationTime ipath
  when (cleanBuild || hasChanged vsrc || time > lastTime) $
    logInfo ("Copying " <> pack (toFilePath ipath))
    *> AIO.copyFile ipath opath
  tell $ dependsOnFile ipath
  pure vsrc

-- | Map a function over a list.
map :: Monad m => (a -> b) -> Recipe m [a] [b]
map f = recipe "map" \v -> pure
  case v of
    Value xs c Nothing   -> value c (Prelude.map f xs)
    Value xs c (Just vs) -> Value (Prelude.map f xs) c (Just (fmap f <$> vs))

-- | Reverse a list.
reverse :: Monad m => Recipe m [a] [a]
reverse = recipe "reverse" \v -> pure (joinValue (Prelude.reverse (splitValue v)))

-- TODO(flupe): change information is incorrect here. if a value changes position, then it is "new" in some sense.
--              maybe we really need difflists

-- | Sort a list using the prelude @sort@.
--   Crucially this takes care of tracking change information in the list.
sort :: (Monad m, Ord a) => Recipe m [a] [a]
sort = recipe "sort" \v -> pure (joinValue (List.sortOn theVal (splitValue v)))

-- | Sort a list using the prelude @sort@.
--   Crucially this takes care of tracking change information in the list.
sortOn :: (Monad m, Ord b) => (a -> b) -> Recipe m [a] [a]
sortOn f = recipe "sortOn" \v -> pure (joinValue (List.sortOn (f . theVal) (splitValue v)))

-- TODO(flupe): make Int argument a task
-- NOTE: not optimal, take/drop both lists?
-- | Return the prefix of length @n@ of the input list.
take :: Monad m => Int -> Recipe m [a] [a]
take n = recipe "take" \v -> pure (joinValue (Prelude.take n (splitValue v)))

-- | Drop the first @n@ elements of the input list.
drop :: Monad m => Int -> Recipe m [a] [a]
drop n = recipe "drop" \v -> pure (joinValue (Prelude.drop n (splitValue v)))

-----------

(!) :: (Monad m, Ord k, AchilleIO m) => Recipe m (Map k v, k) v
(!) = recipe "(!)" \v -> do
  let (vm, vk) = splitValue v
  let vs = splitValue vm
  let vv = if hasChanged vk then value True <$> (theVal vm Map.!? theVal vk)
                            else vs Map.!? theVal vk
  when (isNothing vv) $ fail "oops"
  forward vv
