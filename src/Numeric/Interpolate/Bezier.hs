
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Numeric.Interpolate.Bezier
    ( bezierCurve
    ) where

import AblyPrelude hiding (Num(..))
import AblyPrelude.Lens

import Numeric.Ring
import Numeric.Module
import qualified AblyPrelude.Partial as Partial

import qualified Data.Vector as Vector

bezierCurve :: forall proxy s a. (Module a, Plus a, Partial.Partial s) => proxy s
    -> Vector.Vector a -> Scalar a -> a
bezierCurve p xs r = case Vector.length xs of
    0 -> Partial.partial p
    _ -> go xs
  where
    go :: Vector.Vector a -> a
    go ys = case Vector.length xs of
        1 -> ys Vector.! 0
        _ -> r' Numeric.Module.* go (Vector.init ys) + r Numeric.Module.* go (Vector.tail ys)

    r' :: Scalar a
    r' = one - r


-- | "uncheckedSymmetricalTriangle n" memoizes a function "f" with the following
-- properties:
--
-- f n k = f n (n - k)
-- (k > n) => (f n k = undefined)
--
-- The provided Int specifies the largest value of "n" that the function will
-- ever be called with. The precondition is not checked.
uncheckedSymmetricalTriangle
    :: forall r. Int
    -> ((Int, Int) -> r)
    -> ((Int, Int) -> r)
uncheckedSymmetricalTriangle n f = go
  where
    -- generate a vector indexing from 1
    generate1 :: forall r. Int -> (Int -> r) -> Vector.Vector r
    generate1 n f = Vector.generate n (\x -> f (x + 1))
    -- index into a vector from 1
    (!) :: forall a. Vector.Vector a -> Int -> a
    (!) a b = a Vector.! (b - 1)

    -- generates a symmetrical triangular array with 'n' rows, a la pascal's
    -- triangle
    triangularArray :: Int -> Vector.Vector (Vector.Vector (Int, Int))
    triangularArray n = generate1 n (\r -> generate1 (half r) (\c -> (r, c)))

    memo :: Vector.Vector (Vector.Vector r)
    memo = triangularArray n & over (mapped . mapped) f

    go :: (Int, Int) -> r
    go (r, c)
        | c <= half r = memo ! r ! c
        | otherwise   = memo ! r ! (r - c + 1)

    -- integer division rounded up
    half :: Int -> Int
    half a = div a 2 + mod a 2

memoFix
    :: ((a -> r) -> (a -> r)) -- memoization strategy
    -> ((a -> r) -> (a -> r)) -- unfixed function
    -> a -> r
memoFix memo f =
    let f' = memo (f f')
    in f'

choose :: forall r. (One r) => Int -> Int -> r
choose n = \r -> go (n, r)
  where
    go :: (Int, Int) -> r
    go = memoFix (uncheckedSymmetricalTriangle n) chooseUnfixed

    chooseUnfixed :: ((Int, Int) -> r) -> ((Int, Int) -> r)
    chooseUnfixed loop (n, r)
        | (r == 1) || (r == n) = one
        | otherwise            = loop (n - 1, r - 1) + loop (n - 1, r)

