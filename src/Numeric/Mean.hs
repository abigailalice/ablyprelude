
module Numeric.Mean
    ( agm, ghm, logMean
    , arithmetic, harmonic, geometric
    ) where

import AblyPrelude
import qualified Control.Lens as Lens

arithmetic, harmonic, geometric :: (Floating a) => [a] -> a
arithmetic xs = sum xs / fromIntegral (length xs)
harmonic   xs = fromIntegral (length xs) / Lens.sumOf (traverse . Lens.to recip) xs
geometric  xs = product xs ** recip (fromIntegral $ length xs)

logMean :: (Floating a, Eq a) => a -> a -> a
logMean 0 _ = 0
logMean _ 0 = 0
logMean x y = if x == y then x else (y - x) / (log x - log y)

-- arithmetic-geometric mean, calculated within a given epsilon of the correct
-- value. this is only defined for positive values. however, if i change the
-- geometric mean to:
agm :: (Floating a, Ord a) => a -> a -> a -> a
agm eps a b = if abs (a - b) < eps then a else agm eps a' b'
  where
    a' = (a + b) / 2
    b' = sqrt' (a * b)
    sqrt' x = signum x * sqrt (abs x)

ghm :: (Floating a, Ord a) => a -> a -> a -> a
ghm eps a b = if abs (a - b) < eps then a else ghm eps a' b'
  where
    a' = sqrt' (a * b)
    b' = 2 / (recip a + recip b)
    sqrt' x = signum x * sqrt (abs x)

