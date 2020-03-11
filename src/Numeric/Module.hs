
module Numeric.Module
    ( Module(..)
    ) where

import AblyPrelude
import Numeric

-- |
-- Laws:
--
-- (x /= zero) => (magnitude x * normalize x = x)
class (Times (Scalar a)) => Module a where
    type Scalar a :: Type
    (*) :: Scalar a -> a -> a
    normalize :: a -> a
    magnitude :: a -> Scalar a

