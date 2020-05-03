
module Algebra.Lattice
    ( Lattice(..)
    , Infimum(..)
    , Supremum(..)

    , POrd(..)
    , Ord(..)
    , TOrd(..)
    ) where

import Prelude (Bool, Ordering, Eq)

class POrd a where
    (<) :: a -> a -> Bool
    -- irreflexive: a < a = False
    -- transitive:  a < b and b < c implies a < c
    -- asymmetry:   a < b implies not (b < a)
class (POrd a, Eq a) => Ord a where
    (<=) :: a -> a -> Bool 
    -- a <= b and b <= a implies a = b
class Ord a => TOrd a where
    compare :: a -> a -> Ordering
    -- a <= b or b <= a

class Lattice a where
    (||) :: a -> a -> a
    (&&) :: a -> a -> a


class (Lattice a) => Infimum a where
    inf :: a
class (Lattice a) => Supremum a where
    sup :: a


