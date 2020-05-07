
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Bag
    ( Bag(..)
    , occurrences
    , intersection
    , union
    ) where

import AblyPrelude
import AblyPrelude.Lens
import qualified Data.Semigroup as Semigroup
import qualified Data.Map.Strict as Map
import GHC.Exts

newtype Bag k = Bag { getBag :: Map.Map k Int }

instance Foldable Bag where
    foldMap f (Bag m) = ifoldMap (\k v -> Semigroup.stimes v (f k)) m

instance (Ord k) => IsList (Bag k) where
    type Item (Bag k) = k
    fromList = Bag . Map.fromListWith (+) . fmap u
      where
        u :: k -> (k, Int)
        u x = (x, 1)
    toList = ifoldr go [] . getBag
      where
        go :: k -> Int -> [k] -> [k]
        go k n = mappend (replicate n k)

-- since this always returns a value, using it with 'has' will always rerurn
-- true
occurrences :: (Ord k) => k -> Lens' (Bag k) Int
occurrences k f (Bag m) = Bag <$> at k (fmap to . f . from) m
  where
    to :: Int -> Maybe Int
    to 0 = Nothing
    to n = Just n
    from :: Maybe Int -> Int
    from = maybe 0 id

intersection :: (Ord k) => Bag k -> Bag k -> Bag k
intersection (Bag m) (Bag n) = Bag (Map.intersectionWith min m n)

union :: (Ord k) => Bag k -> Bag k -> Bag k
union (Bag m) (Bag n) = Bag (Map.unionWith max m n)

instance (Ord k) => Semigroup (Bag k) where
    Bag m <> Bag n = Bag (Map.unionWith (+) m n)
instance (Ord k) => Monoid (Bag k) where
    mempty = Bag Map.empty

