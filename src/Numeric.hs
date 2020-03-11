
module Numeric
    ( Plus(..)
    , Zero(..)
    , Negate(..)
    , Times(..)
    , One(..)
    , Recip(..)
    ) where

import Prelude ()

class Plus a where
    (+) :: a -> a -> a

class (Plus a) => Zero a where
    zero :: a

class (Zero a) => Negate a where
    negate :: a -> a
    negate a = zero - a

    (-) :: a -> a -> a
    a - b = a + negate b

class (Zero a) => Times a where
    (*) :: a -> a -> a

class (Times a) => One a where
    one :: a

class (One a) => Recip a where
    recip :: a -> a
    recip a = one / a

    (/) :: a -> a -> a
    a / b = a * recip b



