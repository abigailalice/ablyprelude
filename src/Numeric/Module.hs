
module Numeric.Module
    ( Module(..)
    ) where

import AblyPrelude
import Numeric

-- |
-- Laws:
--
class (Zero (Scalar a)) => Module a where
    type Scalar a :: Type
    (*) :: Scalar a -> a -> a

instance Module UTCTime where
    type Scalar UTCTime 

-- (magnitude x * normalize x = x)
-- normalize zero = zero
-- (x /= zero) => magnitude (normalize x) = one
-- 
-- vector-space norm
class (Module a) => Euclidean a where
    -- the magnitude can be defined on its own for vector spaces, and then
    -- normalize can be defined as 'normalize x = recip (norm x) * x' for
    -- non-zero values, and 'normalize zero = zero' otherwise
    normalize :: a -> a
    magnitude :: a -> Scalar a

