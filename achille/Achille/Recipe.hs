{-# LANGUAGE GADTs, ApplicativeDo, RecordWildCards, OverloadedStrings #-}
module Achille.Recipe
  ( module Achille.Core.Recipe
  , readText
  , readByteString
  , debug
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

import Prelude hiding (reverse, take, drop, map)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Arrow
import Data.Functor (($>))
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.List qualified as List (sortOn)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8)

import Prelude qualified
import Data.Map.Strict qualified as Map

import Achille.IO as AIO
import Achille.Diffable
import Achille.Core.Recipe
  ( Recipe, PrimRecipe
  , recipe, runRecipe
  )
import Achille.Context (Context)
import Achille.DynDeps
import Achille.Path
import Achille.Result (Result)
import Achille.Writable (Writable)

import Achille.Writable qualified as Writable
import Achille.Result qualified as Res
import Achille.Context qualified as Ctx
import Achille.Config qualified as Cfg

-- TODO(flupe): remove getModificationTime invocations when needed

-- | Read a bytestring from file.
readByteString :: (Monad m, AchilleIO m) => Recipe m Path ByteString
readByteString = recipe "readText" \cache v -> do
  path     <- (</>) <$> Res.contentDir <*> pure (theVal v)
  lastTime <- Res.lastTime
  time <- getModificationTime path
  text <- AIO.readFile path
  pure (value (time > lastTime || hasChanged v) text, cache)

-- | Read text from file.
readText :: (Monad m, AchilleIO m) => Recipe m Path Text
readText = recipe "readText" \cache v -> do
  path     <- (</>) <$> Res.contentDir <*> pure (theVal v)
  lastTime <- Res.lastTime
  time <- getModificationTime path
  text <- decodeUtf8 <$> AIO.readFile path
  pure (value (time > lastTime || hasChanged v) text, cache)

-- | Print a message to stdout.
debug :: (Monad m, AchilleIO m) => Recipe m Text ()
debug = recipe "debug" \cache v -> AIO.log (unpack $ theVal v) $> (unit, cache)

-- | Convert a path to a proper absolute URL, including the site prefix.
toURL :: Monad m => Recipe m Path Text
toURL = recipe "Achille.Recipe.toURL" \cache v -> do
  sitePrefix <- reader (Cfg.sitePrefix . Ctx.siteConfig)
  pure (value (hasChanged v) ("/" <> sitePrefix <> pack (toFilePath (theVal v))), cache)

-- | Write something to file, /iff/ this thing has changed since the last run.
write
  :: (Monad m, AchilleIO m, Writable m a)
  => Recipe m (Path, a) Text
write = toURL <<< recipe "write" \cache v -> do
  let (vsrc, vx) = splitValue v
  path       <- (</>) <$> Res.outputDir <*> pure (theVal vsrc)
  cleanBuild <- reader Ctx.cleanBuild
  -- NOTE(flupe): maybe also when the output file is no longer here
  when (cleanBuild || hasChanged v)
    $  AIO.log ("Writing " <> show path)
    *> (lift $ Writable.write path (theVal vx))
  pure (vsrc, cache)

-- | Copies a file to the output path, preserving its name.
copy :: (Monad m, AchilleIO m) => Recipe m Path Text
copy = toURL <<< recipe "copy" \cache vsrc -> do
  ipath <- (</>) <$> Res.contentDir <*> pure (theVal vsrc)
  opath <- (</>) <$> Res.outputDir  <*> pure (theVal vsrc)
  lastTime <- Res.lastTime
  cleanBuild <- reader Ctx.cleanBuild
  -- TODO(flupe): check if file exists
  -- TODO(flupe): check if output file is there?
  time <- getModificationTime ipath
  when (cleanBuild || hasChanged vsrc || time > lastTime) $
    AIO.log ("Copying " <> show ipath) *> AIO.copyFile ipath opath
  tell $ dependsOnFile ipath
  pure (vsrc, cache)

-- | Map a function over a list.
map :: Applicative m => (a -> b) -> Recipe m [a] [b]
map f = recipe "map" \cache v -> pure . (, cache) $
  case v of
    Value xs c Nothing   -> value c (Prelude.map f xs)
    Value xs c (Just vs) -> Value (Prelude.map f xs) c (Just (fmap f <$> vs))

-- | Reverse a list.
reverse :: Applicative m => Recipe m [a] [a]
reverse = recipe "reverse" \cache v -> pure (joinValue (Prelude.reverse (splitValue v)), cache)

-- TODO(flupe): change information is incorrect here. if a value changes position, then it is "new" in some sense.
--              maybe we really need difflists

-- | Sort a list using the prelude @sort@.
--   Crucially this takes care of tracking change information in the list.
sort :: (Applicative m, Ord a) => Recipe m [a] [a]
sort = recipe "sort" \cache v -> pure (joinValue (List.sortOn theVal (splitValue v)), cache)

-- | Sort a list using the prelude @sort@.
--   Crucially this takes care of tracking change information in the list.
sortOn :: (Applicative m, Ord b) => (a -> b) -> Recipe m [a] [a]
sortOn f = recipe "sortOn" \cache v -> pure (joinValue (List.sortOn (f . theVal) (splitValue v)), cache)

-- TODO(flupe): make Int argument a task
-- NOTE: not optimal, take/drop both lists?
-- | Return the prefix of length @n@ of the input list.
take :: (Applicative m) => Int -> Recipe m [a] [a]
take n = recipe "take" \cache v -> pure (joinValue (Prelude.take n (splitValue v)), cache)

-- | Drop the first @n@ elements of the input list.
drop :: Applicative m => Int -> Recipe m [a] [a]
drop n = recipe "drop" \cache v -> pure (joinValue (Prelude.drop n (splitValue v)), cache)

-----------

(!) :: (Applicative m, Ord k) => Recipe m (Map k v, k) v
(!) = recipe "(!)" \cache v -> do
  let (vm, vk) = splitValue v
  let vs = splitValue vm
  let vv = if hasChanged vk then value True (theVal vm Map.! theVal vk)
                              else vs Map.! theVal vk
  pure (vv, cache)
