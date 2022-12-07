module Achille.Recipe.Base where

import Achille.Recipe
import Achille.IO (AchilleIO)
import Achille.IO qualified as AchilleIO

-- | Log a message to stdout.
debug :: (Functor m, AchilleIO m) => String -> Task m ()
debug = liftD . AchilleIO.log
