
module Numeric.Ring
    ( Plus(..)
    , Zero(..)
    , Negate(..)
    , Times(..)
    , One(..)
    , Recip(..)

    , WrappedNum(..)
    ) where

import Prelude (Int, Double, Integer)
import qualified Prelude

class Plus a where
    (+) :: a -> a -> a
infixl 6 +

class (Plus a) => Zero a where
    zero :: a

class (Zero a) => Negate a where
    negate :: a -> a
    negate a = zero - a

    (-) :: a -> a -> a
    a - b = a + negate b

class (Zero a) => Times a where
    (*) :: a -> a -> a
infixl 7 *

class (Times a) => One a where
    one :: a

class (One a) => Recip a where
    recip :: a -> a
    recip a = one / a

    (/) :: a -> a -> a
    a / b = a * recip b

newtype WrappedNum a = WrappedNum a

instance (Prelude.Num a) => Plus (WrappedNum a) where
    WrappedNum a + WrappedNum b = WrappedNum (a Prelude.+ b)

instance (Prelude.Num a) => Zero (WrappedNum a) where
    zero = WrappedNum 0

instance (Prelude.Num a) => Negate (WrappedNum a) where
    WrappedNum a - WrappedNum b = WrappedNum (a Prelude.- b)
    negate (WrappedNum a) = WrappedNum (Prelude.negate a)

instance (Prelude.Num a) => One (WrappedNum a) where
    one = WrappedNum 1

instance (Prelude.Num a) => Times (WrappedNum a) where
    WrappedNum a * WrappedNum b = WrappedNum (a Prelude.* b)

instance (Prelude.Fractional a) => Recip (WrappedNum a) where
    recip (WrappedNum a) = WrappedNum (Prelude.recip a)
    WrappedNum a / WrappedNum b = WrappedNum (a Prelude./ b)

deriving via (WrappedNum Int) instance Plus Int
deriving via (WrappedNum Int) instance Zero Int
deriving via (WrappedNum Int) instance Negate Int
deriving via (WrappedNum Int) instance Times Int
deriving via (WrappedNum Int) instance One Int

deriving via (WrappedNum Integer) instance Plus Integer
deriving via (WrappedNum Integer) instance Zero Integer
deriving via (WrappedNum Integer) instance Negate Integer
deriving via (WrappedNum Integer) instance Times Integer
deriving via (WrappedNum Integer) instance One Integer

deriving via (WrappedNum Double) instance Plus Double
deriving via (WrappedNum Double) instance Zero Double
deriving via (WrappedNum Double) instance Negate Double
deriving via (WrappedNum Double) instance Times Double
deriving via (WrappedNum Double) instance One Double
deriving via (WrappedNum Double) instance Recip Double
