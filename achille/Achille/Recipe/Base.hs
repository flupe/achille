module Achille.Recipe.Base where

import Achille.Recipe
import Achille.IO (AchilleIO)
import Achille.IO qualified as AchilleIO

debug :: (Functor m, AchilleIO m) => String -> Task m ()
debug = liftD . AchilleIO.log
