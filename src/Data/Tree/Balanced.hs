
module Data.Tree.Balanced
    ( Tree(..)
    , naryFold
    ) where

import AblyPrelude

-- compare with
--
-- Free f a = Free (f (Free f a)) | Pure a
-- Tree f a = Tree (Tree n
--
-- Tree n a = a | n a | n (n a) | n (n (n a)) | ...
data Tree n a
    = Tree (Tree n (n a))
    | Leaf a
    deriving (Functor, Foldable, Traversable)

naryFold :: forall n a b. (Functor n) => (a -> b) -> (n b -> b) -> Tree n a -> b
naryFold g _ (Leaf a) = g a
naryFold g f (Tree a) = a & naryFold (fmap g) (fmap f) & f


