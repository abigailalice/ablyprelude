
module Numeric.Poly
    ( Poly(..)
    , runPoly
    , diff
    ) where

import Prelude
import Control.Lens as Lens

data Poly a = Poly
    { zero   :: a
    , coeffs :: [a]
    }

runPoly :: (Num a) => Poly a -> a -> a
runPoly (Poly x0 cs) x = foldr go 0 cs
  where
    dx = x - x0
    go c z = c + z * dx

diff :: forall a. (Num a) => Poly a -> Poly a
diff (Poly x0 cs) = Poly x0 (drop 1 (Lens.imap scalarMultiply cs))
  where
    scalarMultiply :: Int -> a -> a
    scalarMultiply a b = fromIntegral a * b

