
module Data.List.Church
    ( List(..)
    ) where

import Prelude

newtype List a = List (forall r. (r -> a -> r) -> r -> r)

instance Semigroup (List a) where
    List a <> List b = List (\cns nil -> a cns (b cns nil))

instance Monoid (List a) where
    mempty = List (\_ nil -> nil)

instance Functor List where
    fmap f (List m) = List (\cns nil -> m (\r a -> cns r (f a)) nil)

