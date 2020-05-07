
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Module
    ( Module(..)
    , Metric(..)
    ) where

import qualified Prelude

import Numeric.Ring
import AblyPrelude hiding (Num(..), Fractional(..))

-- |
-- Laws:
--
-- (x /= zero) => (magnitude x * normalize x = x)
--
-- laws can bbe given for * from the module laws
--
class (One (Scalar a), Negate (Scalar a)) => Module a where
    type Scalar a :: Type
    (*) :: Scalar a -> a -> a
infixl 7 *

-- A metric which agrees with the module operator, so that the following hold
--
-- d x y = 0 iff x = y
-- d x y = d y x
-- d x y <= d x z + d z y
-- (x /= zero) => (norm x * normalize x = x)
-- norm (a * b) = norm a * norm b
--   implies "Scalar (Scalar a) = Scalar a"
--
-- the last law could hold if we define "normalize zero = zero"
class (Negate a, Module a, Recip (Scalar a)) => Metric a where
    d :: a -> a -> Scalar a
    d x y = norm (x - y)

    norm :: a -> Scalar a
    norm = d zero

    normalize :: a -> a
    normalize a = recip (norm a) Numeric.Module.* a

instance (Prelude.Num a, One a, Negate a) => Module (WrappedNum a) where
    type Scalar (WrappedNum a) = a
    a * WrappedNum b = WrappedNum (a Prelude.* b)

instance Module Double where
    type Scalar Double = Double
    (*) = (Prelude.*)

