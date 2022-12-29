-- | Top-level module for achille, providing the CLI and task runner.
module Achille
    ( achille
    , achilleWith
    , Config (..)
    ,  module Achille.Task
    ) where

import Achille.Config
import Achille.CLI (achille, achilleWith)
import Achille.Task
