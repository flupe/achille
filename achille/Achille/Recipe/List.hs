-- | Some usual recipes to process lists, keeping track of change information.
module Achille.Recipe.List where

import Achille.Recipe (Recipe(Recipe), vArr)
import Data.Bifunctor (bimap)
import Data.List qualified as List (sortOn)

-- | Sort a list using the prelude @sort@.
-- Crucially this takes care of tracking change information in the list.
sort :: (Applicative m, Ord a) => Recipe m [a] [a]
sort = vArr $ unzip . List.sortOn fst . uncurry zip

-- | Sort a list using the prelude @sort@.
-- Crucially this takes care of tracking change information in the list.
sortOn :: (Applicative m, Ord b) => (a -> b) -> Recipe m [a] [a]
sortOn f = vArr $ unzip . List.sortOn (f . fst) . uncurry zip

-- | Return the prefix of length @n@ of the input list.
take :: (Applicative m) => Int -> Recipe m [a] [a]
take n = vArr $ bimap (Prelude.take n) (Prelude.take n)

-- | Drop the first @n@ elements of the input list.
drop :: Applicative m => Int -> Recipe m [a] [a]
drop n = vArr $ bimap (Prelude.drop n) (Prelude.drop n)

-- | Split the input list into lists of at most @n@ elements.
chunks :: Applicative m => Int -> Recipe m [a] [[a]]
chunks k = vArr $ bimap chunks' chunks'
  where chunks' :: [a] -> [[a]]
        chunks' [] = []
        chunks' xs = Prelude.take k xs : chunks' (Prelude.drop k xs)
