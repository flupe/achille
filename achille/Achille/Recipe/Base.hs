module Achille.Recipe.Base where

import Data.Functor (($>))

import Achille.Diffable (unitV)
import Achille.Recipe (Recipe(..))
import Achille.Writable
import Achille.IO (AchilleIO)
import Achille.IO qualified as AchilleIO

-- | Log a message to stdout.
debug :: (Functor m, AchilleIO m) => Recipe m String ()
debug = Recipe \ctx cache (msg, _) -> AchilleIO.log msg $> (unitV, cache)

-- | Write something to a file.
writeFile :: Writable m a => Recipe m (FilePath, a) ()
writeFile = undefined
