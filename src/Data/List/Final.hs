
module Data.List.Final
    ( List(..)
    , reverse
    , partition
    , partitionEither
    , partitionThese
    , uncons
    , unsnoc
    , head
    , last
    , First1(..)
    , Last1(..)
    ) where

import Prelude hiding (reverse, last, head)
import Data.Monoid (Dual(..), First(..), Last(..))
import Control.Applicative (Alternative(..))
import Control.Lens hiding (List, uncons, unsnoc)
import Data.These hiding (partitionThese)
import Witherable

newtype List a = List (forall w. Monoid w => (a -> w) -> w)

instance Functor List where
    fmap f (List r) = List (\a -> r (a . f))

instance Applicative List where
    pure a = List (\r -> r a)
    List f <*> List x = List (\k -> f (\g -> x (k . g)))

instance Monad List where
    List x >>= f = x f

instance Alternative List where
    empty = mempty
    (<|>) = (<>)

instance Semigroup (List a) where
    List a <> List b = List (a <> b)

instance Monoid (List a) where
    mempty = List (const mempty)

reverse :: List a -> List a
reverse (List a) = List (dimap (fmap Dual) getDual a)

instance Foldable List where
    foldMap f (List r) = r f

instance Filterable List where
    catMaybes (List r) = List (lmap (maybe mempty) r)

partition :: forall a. (a -> Bool) -> List a -> (List a, List a)
partition p (List m) = m $ (\a -> if p a then (pure a, mempty) else (mempty, pure a))

partitionEither :: List (Either a b) -> (List a, List b)
partitionEither (List m) = m \case
    Left e -> (pure e, mempty)
    Right a -> (mempty, pure a)

partitionThese :: List (These a b) -> (List a, List b)
partitionThese (List m) = m \case
    This e -> (pure e, mempty)
    These e a -> (pure e, pure a)
    That a -> (mempty, pure a)

newtype First1 a = First1 { unFirst1 :: Maybe (a, List a) }

instance Semigroup (First1 a) where
    First1 Nothing <> First1 a = First1 a
    First1 xs <> First1 Nothing = First1 xs
    First1 (Just (x, xs)) <> First1 (Just (y, ys)) = First1 $ Just (x, xs <> pure y <> ys)
instance Monoid (First1 a) where
    mempty = First1 Nothing
instance Foldable First1 where
    foldMap _ (First1 Nothing) = mempty
    foldMap f (First1 (Just (a, as))) = f a <> foldMap f as
instance Functor First1 where
    fmap _ (First1 Nothing) = First1 Nothing
    fmap f (First1 (Just (a, as))) = First1 (Just (f a, fmap f as))


uncons :: List a -> Maybe (a, List a)
uncons (List m) = unFirst1 $ m \a -> First1 (Just (a, mempty))

head :: List a -> Maybe a
head (List m) = getFirst $ m (First . Just)

last :: List a -> Maybe a
last (List m) = getLast $ m (Last . Just)

newtype Last1 a = Last1 { unLast1 :: Maybe (List a, a) }
instance Semigroup (Last1 a) where
    Last1 a <> Last1 Nothing = Last1 a
    Last1 Nothing <> Last1 a = Last1 a
    Last1 (Just (xs, x)) <> Last1 (Just (ys, y)) = Last1 $ Just (xs <> pure x <> ys, y)
instance Monoid (Last1 a) where
    mempty = Last1 Nothing

unsnoc :: List a -> Maybe (List a, a)
unsnoc (List m) = unLast1 $ m \a -> Last1 (Just (mempty, a))



