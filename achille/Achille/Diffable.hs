module Achille.Diffable
  ( HasChanged
  , Diff
  , Value
  , diff
  , hasChanged
  , unitV
  , D
  , splitList
  , joinList
  , splitPair
  , joinPair
  ) where

type HasChanged = Bool
type Value a = (a, Diff a)
type Diff a = (HasChanged, Maybe (D a))

type family D a

-- | Create a diff out of thin air.
diff :: a -> Bool -> Diff a
diff _ b = (b, Nothing)

-- | Whether a value has changed.
hasChanged :: Value a -> Bool
hasChanged (_, (b, _)) = b

unitV :: Value ()
unitV = ((), diff () False)


type instance D ()     = ()
type instance D (a, b) = (Diff a, Diff b)
type instance D [a]    = [Diff a]

-- | For types that extend the type family @D@, it's possible
-- to retrieve inner diffing information.
splitList :: Value [a] -> [Value a]
splitList (xs, (changed, Nothing)) = [ (x, diff x changed) | x <- xs ]
splitList (xs, (_      , Just ds)) = zip xs ds

-- TODO
joinList :: [Value a] -> Value [a]
joinList xs = (fst <$> xs, undefined)

-- | Same here, we retrieve diffing information of each component of the pair.
splitPair :: Value (a, b) -> (Value a, Value b)
splitPair ((x, y), (changed, Nothing)) = ((x, diff x changed), (y, diff y changed))
splitPair ((x, y), (_, Just (dx, dy))) = ((x, dx), (y, dy))

joinPair :: Value a -> Value b -> Value (a, b)
joinPair (x, (cx, dx)) (y, (cy, dy)) =
  ((x, y), (cx || cy, Just ((cx, dx), (cy, dy))))
