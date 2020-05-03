
-- these are unparameterized functions

module Neural.Layer.Activation
    -- scalar functions: a -> a
    ( smoothLog, smoothExp
    , softPlus
    -- vectorized functions: f a -> f a
    , normalize
    -- reducers: f a -> a
    , logSumExp, logSumExpPlus
    ) where

import AblyPrelude

import GHC.Float
import qualified Control.Lens as Lens


-- a smooth defined everywhere approximation of the log function. For values
-- with a small magnitude 'smoothLog' ~ 'smoothExp' ~ 'id'
smoothLog, smoothExp :: (Floating a) => a -> a
smoothLog x = signum x * log1p (abs x)
smoothExp x = signum x * expm1 (abs x)

-- a smooth rectified linear function
softPlus :: (Floating a) => a -> a
softPlus = log1p . exp

-- normalize, softMax is a vectorized sigmoid
-- 'softArgMax' serves as a generalization of 'expit' to multiple arguments, as
-- well as an smooth approximation of the argmax function. Traditionally it's
-- referred to as 'softMax', though 'logSumExp' is more accurately a smooth
-- maximum function
normalize :: (Floating a, Functor f, Foldable f) => f a -> f a
normalize x = fmap (/ x') x
  where
    x' = sqrt (sum (fmap sqr x))
    sqr y = y * y

-- smooth approximation of the maximum function
-- 'logSumExpPlus' is equivalent to 'softPlus'
logSumExp, logSumExpPlus :: (Floating a, Functor f, Foldable f, Ord a, Num a)
    => f a -> a
logSumExp xs = xs
    & Lens.sumOf (Lens.folded . Lens.to (subtract x) . Lens.to exp)
    & log
    & (+) x
  where
    x = maximum xs
logSumExpPlus xs = xs
    & Lens.sumOf (Lens.folded . Lens.to (subtract x) . Lens.to exp)
    & (+) (exp (negate x))
    & log
    & (+) x
  where
    x = max 0 (maximum xs)


