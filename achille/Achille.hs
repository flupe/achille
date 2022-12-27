-- | Top-level module for achille, providing the CLI and task runner.
module Achille
    ( module Achille.Syntax
    , achille
    , achilleWith
    , Config (..)
    ) where

import Achille.Config
import Achille.CLI    (achille, achilleWith)
import Achille.Recipe (Recipe)
import Achille.Syntax
import Achille.Task   (Task)
