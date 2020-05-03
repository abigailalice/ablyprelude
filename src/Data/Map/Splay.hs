
module Data.Map.Splay
    ( Map(..)
    , singleton
    , Data.Map.Splay.empty
    , fromList
    , fromAscList
    , ifilterMapM
    ) where

import AblyPrelude
import AblyPrelude.Lens hiding (Empty)
import GHC.Generics
import qualified Control.Lens as Lens

data Map k v
    = Empty
    | Bin (Map k v) k v (Map k v)
    deriving (Functor, Foldable, Traversable)

data Zipper k v = Zipper
    { _breadCrumbs :: [Up k v]
    , focus       :: Map k v
    } deriving (Generic)
breadCrumbs :: Lens' (Zipper k v) [Up k v]
breadCrumbs f (Zipper b t) = flip Zipper t <$> f b

data Up k v
    = UpLeft (Map k v) k v
    | UpRight k v (Map k v)

type instance Lens.IxValue (Map k v) = v
type instance Lens.Index (Map k v) = k

instance (Ord k) => Lens.Ixed (Map k v) where
    ix :: k -> Lens.Traversal' (Map k v) v
    ix k f = Lens.at k (traverse f)

instance (Ord k) => Lens.At (Map k v) where
    at :: k -> Lens.Lens' (Map k v) (Maybe v)
    at k0 f x = case path x of
        Zipper xs Empty -> f Nothing Lens.<&> \case
            Just v' -> splayOnNode xs Empty k0 v' Empty
            Nothing -> splayOnParent xs Empty
        Zipper xs (Bin l _ v r) -> f (Just v) Lens.<&> \case
            Just v' -> splayOnNode xs l k0 v' r
            Nothing -> splayOnParent xs (mergeUnchecked l r)
      where
        path :: Map k v -> Zipper k v
        path (Bin l k1 v r)
            | k1 < k0 = path l Lens.& over breadCrumbs (UpRight k1 v r :)
            | k1 > k0 = path r Lens.& over breadCrumbs (UpLeft l k1 v :)
        path r = Zipper [] r


singleton :: k -> v -> Map k v
singleton k v = Bin Empty k v Empty

empty :: Map k v
empty = Empty

ifilterMapM :: forall m k v v'. (Applicative m)
    => (k -> v -> m (Maybe v')) -> Map k v -> m (Map k v')
ifilterMapM f = go
  where
    go :: Map k v -> m (Map k v')
    go Empty = pure Empty
    go (Bin l k v u) = merge k <$> go l <*> f k v <*> go u

    merge :: k -> Map k v' -> Maybe v' -> Map k v' -> Map k v'
    merge _ l Nothing u  = mergeUnchecked l u
    merge k l (Just v) u = Bin l k v u

-- we could splay the in-order predecessor rather than swap it to the top, which
-- would cause the tree to become more balanced 
mergeUnchecked :: Map k v -> Map k v -> Map k v
mergeUnchecked (Bin l'' k'' v'' r) u@(Bin _ _ _ _) =
    let (x, k', v') = deleteMax l'' k'' v'' r
    in Bin x k' v' u
  where
    deleteMax :: Map k v -> k -> v -> Map k v -> (Map k v, k, v)
    deleteMax l k v Empty = (l, k, v)
    deleteMax l k v (Bin l' k' v' r')
        = deleteMax l' k' v' r' Lens.& over _1 (Bin l k v)
mergeUnchecked Empty r = r
mergeUnchecked l     _ = l

-- O(n) ~ O(n^2), depending on how well the list is sorted
fromList :: forall k v. (Ord k) => [(k, v)] -> Map k v
fromList = foldr go Empty
  where
    go :: (k, v) -> Map k v -> Map k v
    go (k, v) xs = xs Lens.& at k .~ Just v

fromAscList :: [(k, v)] -> Map k v
fromAscList = foldr (\(k, v) r -> Bin Empty k v r) Empty

splayOnNode :: [Up k v] -> Map k v -> k -> v -> Map k v -> Map k v
-- zig-zig
splayOnNode (UpRight k v r : UpRight k' v' r' : ps) ln kn vn rn
    = splayOnNode ps ln kn vn (Bin rn k v (Bin r k' v' r'))
splayOnNode (UpLeft _ k v : UpLeft l' k' v' : ps) ln kn vn rn
    = splayOnNode ps (Bin (Bin l' k' v' rn) k v ln) kn vn rn
-- zig-zag
splayOnNode (UpLeft l k v : UpRight k' v' r' : ps) ln kn vn rn
    = splayOnNode ps (Bin l k v ln) kn vn (Bin rn k' v' r')
splayOnNode (UpRight k v r : UpLeft l' k' v' : ps) ln kn vn rn
    = splayOnNode ps (Bin l' k' v' ln) kn vn (Bin rn k v r)
-- zig
splayOnNode [UpLeft l k v ] ln kn vn rn = Bin (Bin l k v ln) kn vn rn
splayOnNode [UpRight k v r] ln kn vn rn = Bin ln kn vn (Bin rn k v r)
splayOnNode []              ln kn vn rn = Bin ln kn vn rn

-- splays on the parent, inserting a given splaytree as the parent's missing
-- node
splayOnParent :: [Up k v] -> Map k v -> Map k v
splayOnParent []       y = y
splayOnParent (x : xs) y =
    let Bin l k v r = fromUp x y
    in splayOnNode xs l k v r
  where
    fromUp :: Up k v -> Map k v -> Map k v
    fromUp (UpLeft l k v) r = Bin l k v r
    fromUp (UpRight k v r) l = Bin l k v r

