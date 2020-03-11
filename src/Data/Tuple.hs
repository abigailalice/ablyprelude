
module Data.Tuple
    ( Tuple(..)
    , Union(..)
    ) where

import Data.Kind

-- a heterogeneous list of the s
-- could also have a heterogeneous list of 

data Tuple :: (k -> Type) -> [k] -> Type where
    Nil :: Tuple f '[]
    Cons :: f a -> Tuple f as -> Tuple f (a ': as)

data Union :: (k -> Type) -> [k] -> Type where
    This :: f a -> Union f (a ': as)
    That :: Union f as -> Union f (a ': as)

