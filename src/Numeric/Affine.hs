
module Numeric.Affine
    ( Affine(..)
    ) where

import AblyPrelude
import Numeric

class (Negate (Delta a)) => Affine a where
    type Delta a :: Type
    (+) :: Delta a -> a -> a
    (-) :: a -> a -> Delta a


