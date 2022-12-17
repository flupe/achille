{-# LANGUAGE ApplicativeDo #-}
module Achille.Recipe.Base where

import Control.Monad (when)
import Data.Functor  (($>))

import Achille.Diffable (unitV, splitPair, hasChanged)
import Achille.Recipe   (Recipe(..))
import Achille.Writable (Writable)
import Achille.IO       (AchilleIO)
import Achille.IO       qualified as AchilleIO
import Achille.Writable qualified as W

-- | Log a message to stdout.
debug :: (Functor m, AchilleIO m) => Recipe m String ()
debug = Recipe \ctx cache (msg, _) -> AchilleIO.log msg $> (unitV, cache)

-- | Write something to a file.
write :: (Applicative m, Writable m a) => Recipe m (FilePath, a) FilePath
write = Recipe \ctx cache v -> do
  let (vsrc@(src, _), (x, _)) = splitPair v
  when (hasChanged v) $ W.write src x
  pure (vsrc, cache)
