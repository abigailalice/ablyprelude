
{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiWayIf #-}

module Numeric.Interpolate.Bezier
    ( bezierCurve
    , bezierCurve'
    , basisSpline
    ) where

import AblyPrelude
import AblyPrelude.Lens

import qualified Data.List as List

import qualified Numeric.Ring as Numeric
import qualified Numeric.Module as Scalar
import qualified AblyPrelude.Partial as Partial

import qualified Data.Vector as Vector

bezierCurve :: forall proxy s a. (Scalar.Module a, Numeric.Plus a, Num (Scalar.Scalar a), Partial.Partial s) => proxy s
    -> Vector.Vector a -> Scalar.Scalar a -> a
bezierCurve p xs r = case Vector.length xs of
    0 -> Partial.partial p
    _ -> go xs
  where
    go :: Vector.Vector a -> a
    go ys = case Vector.length xs of
        1 -> ys Vector.! 0
        _ -> r' Scalar.* go (Vector.init ys) Numeric.+ r Scalar.* go (Vector.tail ys)

    r' :: Scalar.Scalar a
    r' = Numeric.one - r

-- https://pages.mtu.edu/~shene/COURSES/cs3621/NOTES/spline/B-spline/bspline-curve.html
basisSpline :: forall a. (Numeric.Zero a, Scalar.Module a, Ord (Scalar.Scalar a), Fractional (Scalar.Scalar a))
    => Vector.Vector (Scalar.Scalar a) -> Vector.Vector a -> Scalar.Scalar a -> a
basisSpline ks cs u = cs
    & imap (\i c -> bSplineBasis i p u Scalar.* c)
    & foldl' (Numeric.+) Numeric.zero
  where
    p :: Int
    p = Vector.length ks - Vector.length cs - 1

    bSplineBasis :: Int -> Int -> Scalar.Scalar a -> Scalar.Scalar a
    bSplineBasis i 1 x
        | (t i <= x) && (x < t (i + 1)) = 1
        | otherwise                     = 0
    bSplineBasis i k x =
        let a = (x - t i) / (t (i + k) - t i)
            b = (t (i + k + 1) - x) / (t (i + k + 1) - t (i + 1))
        in a * bSplineBasis i (k - 1) x + b * bSplineBasis (i + 1) (k - 1) x

    t :: Int -> Scalar.Scalar a
    t n = ks Vector.! n

bezierCurve' :: forall a. (Scalar.Module a, Numeric.Zero a, Num (Scalar.Scalar a))
    => [a] -> [Scalar.Scalar a] -> [a]
bezierCurve' ps = fmap \x ->
    let xs  = iterate (* x ) 1 & take n
        xs' = iterate (* x') 1 & take n & reverse
          where
            x' = 1 - x
    in List.zipWith4 multiply bs xs xs' ps & foldl' (Numeric.+) Numeric.zero
  where
    -- this does nothing interesting but for type conversions
    multiply :: Integer -> Scalar.Scalar a -> Scalar.Scalar a -> a -> a
    multiply a b c d = (fromIntegral a * b * c) Scalar.* d

    n :: Int
    n = length ps

    bs :: [Integer]
    bs = binomialCoefficients' (fromIntegral n)


-- this is O(n^2), though performs only additions. it could be parallelized
-- somewhat, and also could take advantage of symmetry
binomialCoefficients :: Int -> [Integer]
binomialCoefficients 0 = [1]
binomialCoefficients n = binomialCoefficients (n - 1) & nextRow
  where
    nextRow :: [Integer] -> [Integer]
    nextRow = (1 :) . go
      where
        go (x0 : xs@(x1 : _)) = (x0 + x1) : go xs
        go _ = [1]

-- this is O(n), though performs integer division. for sufficiently small ints
-- this could use doubles and then convert them
--
-- For inputs in the range [0 .. 1020] this algorithm can use floating be used with doubles using
-- floating point division 
-- For sufficiently small inputs [0 .. 1020] incl
binomialCoefficients' :: Integer -> [Integer]
binomialCoefficients' = \n -> if
    | n <= 1020 -> (fmap floor . binomDouble . fromInteger) n
    | otherwise -> binomInteger n
  where
    binomInteger :: Integer -> [Integer]
    binomInteger n = scanl go 1 [1 .. n]
      where
        go z k = z * (n + 1 - k) `quot` k

    binomDouble :: Double -> [Double]
    binomDouble n = scanl go 1 [1 .. n]
      where
        go z k = (z * (n + 1 - k)) / k

