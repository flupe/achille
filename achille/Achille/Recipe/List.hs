-- | Some usual recipes to process lists, keeping track of change information.
module Achille.Recipe.List where

import Achille.Diffable (splitList, joinList)
import Achille.Recipe (Recipe(Recipe), vArr)
import Data.Bifunctor (bimap)
import Data.List qualified as List (sortOn)

-- | Sort a list using the prelude @sort@.
-- Crucially this takes care of tracking change information in the list.
sort :: (Applicative m, Ord a) => Recipe m [a] [a]
sort = vArr $ joinList . List.sortOn fst . splitList

-- | Sort a list using the prelude @sort@.
-- Crucially this takes care of tracking change information in the list.
sortOn :: (Applicative m, Ord b) => (a -> b) -> Recipe m [a] [a]
sortOn f = vArr $ joinList . List.sortOn (f . fst) . splitList

-- NOTE: not optimal, take/drop both lists?
-- | Return the prefix of length @n@ of the input list.
take :: (Applicative m) => Int -> Recipe m [a] [a]
take n = vArr $ joinList . Prelude.take n . splitList

-- | Drop the first @n@ elements of the input list.
drop :: Applicative m => Int -> Recipe m [a] [a]
drop n = vArr $ joinList . Prelude.drop n . splitList

-- -- | Split the input list into lists of at most @n@ elements.
-- chunks :: Applicative m => Int -> Recipe m [a] [[a]]
-- chunks k = vArr $ joinList . chunks' . splitList
--   where chunks' :: [a] -> [[a]]
--         chunks' [] = []
--         chunks' xs = Prelude.take k xs : chunks' (Prelude.drop k xs)
