
{-# LANGUAGE TupleSections #-}

module Numeric.Interpolate.MonotonicCubic
    ( Boundary(..)
    , calculateDistribution
    ) where

import AblyPrelude hiding (First, Last)
import AblyPrelude.Lens
import AblyPrelude.Data
import AblyPrelude.Development (undefined)

import qualified Data.List as List
import qualified Numeric.Poly as Poly
import qualified Data.Map as Map
import qualified Data.Vector as Vector


data Boundary a
    -- use known first derivatives at the end-points
    = Known a a
    | Parabola
    | Exponential a a
    | NoCurve

linearInterpolate :: Floating a => Rational -> a -> a -> a
linearInterpolate p a b = (1 - p) * a + p * b
  where
    a * b = fromRational a AblyPrelude.* b


calculateDistribution
    :: (Floating a, Ord a, Enum a)
    => (Rational -> a -> a -> a)
    -> Int
    -> Boundary a
    -> [a]
    -> [(a, a, a)]
calculateDistribution weightedAverage numSamples boundaryConditions
    = monotonicInterpolate numSamples boundaryConditions
    . Vector.fromList
    . discreteCDF weightedAverage
    . List.sort

-- for a list of n elements ranging from [a,b] the probability of a value being
-- the largest seen so far is 1/(n+1), likewise for the smallest seen so far.
-- i'm not sure how best to include this fact in this function. this is
-- specific to non-discrete random variables
--
-- i think that means the first value should be 1/(n+1) if we're using a
-- less-than interpretation, so averaging those values we should set the
-- smallest value to (1/(n+1) + 1/n)/2, and the largest to its complement. so
-- we want a mean function which gives this solution
discreteCDF
    :: forall k. (Ord k, Fractional k)
    => (Rational -> k -> k -> k) -> [k] -> [(k, k)]
discreteCDF f
    = Map.toList
    . uncurry normalize
    . integrate 
    . toMap
  where
    toMap :: (Ord k) => [k] -> Map k Int
    toMap = Map.fromListWith (+) . fmap (, 1)

    normalize :: (Functor f, Fractional k) => k -> f k -> f k
    normalize z = fmap (/ z)

    integrate :: Map k Int -> (k, Map k k)
    integrate = Map.mapAccumWithKey go 0 . indexed
      where
        -- it may also be reasonable to return (z', z), which returns the LT
        -- percentiles rather than LTE. some average of the two may also be
        -- sensible
        go :: k -> k -> (Rational, Int) -> (k, k)
        go z k (p, v) =
            let z' = z + fromIntegral v * k
            in (z', f p z z')

    -- adds the percentile index of each element in the map, so that the
    -- first element has index 0 and the last has index 1
    indexed :: Map k a -> Map k (Rational, a)
    indexed m =
        let s :: Int
            s = length m - 1
        in m & iover traversed (\i a -> (i // s, a))

    (//) :: (Fractional a) => Int -> Int -> a
    a // b = fromIntegral a / fromIntegral b

-- Given an ordered list of (interval,function) pairs and on ordered list of
-- points, run the functions on each point, dropping any points that don't lie
-- in an interval. The precondition (that both lists are in ascending order) is
-- not checked
runPiecewiseUnchecked
    :: forall f x y. (Ord x) => (f -> x -> y) -> [(x, x, f)] -> [x] -> [y]
runPiecewiseUnchecked f intervals points = go intervals points
  where
    go :: [(x, x, f)] -> [x] -> [y]
    go (pss@((lb, ub, poly) : ps)) (xss@(x : xs)) = case between lb x ub of
        -- the point occurs before the interval. because every earlier interval
        -- must have been encountered this means no interval includes the point.
        -- thus we ignore the point.
        LT -> go pss xs
        -- the interval overlaps the point
        EQ -> f poly x : go pss xs
        -- the point occurs after the interval, as does every future point, so
        -- drop the interval
        GT -> go ps xss
    go _ _ = []
    between :: (Ord a) => a -> a -> a -> Ordering
    between a b c
        | b < a                = LT
        | (a <= b) && (b <= c) = EQ
        | c < b                = GT

monotonicInterpolate :: forall a. (Floating a, Ord a, Enum a)
    => Int -> Boundary a -> Vector.Vector (a, a) -> [(a, a, a)]
monotonicInterpolate n b xs =
    let x1, xn :: a
        x1 = xs Vector.! 0 & fst
        xn = xs Vector.! (Vector.length xs - 1) & fst

        polys :: [(a, a, Poly.Poly a)]
        polys = interpolationArtifacts b xs

        points :: [a]
        points = equallySpaced n x1 xn

    in runPiecewiseUnchecked run (toList polys) points
  where
    run :: Poly.Poly a -> a -> (a, a, a)
    run f x = (x, Poly.runPoly f x, Poly.runPoly (Poly.diff f) x)

    equallySpaced :: Int -> a -> a -> [a]
    equallySpaced 0 _  _  = []
    equallySpaced n x0 xn =
        let step = (xn - x0) / fromIntegral n
        in [x0, x0 + step .. xn]

-- Given a set of (x,y) pairs returns a vector of coefficients (a,b,c,d) for
-- a third degree piecewise defined polynomial over each internal interval.
--
-- Thus, if the provided vector has n points the returned one has n-1 4-tuples
--
-- URL: http://adsabs.harvard.edu/full/1990A%26A...239..443S
-- Title: A Simple Method for Monotonic Interpolation in One Dimension
-- Authors: Steffen, M.
-- Journal: Astronomy and Astrophysics, Vol. 239, NO. NOV(II), P. 443, 1990
-- Bibliographic Code: 1990A&A...239..443S
--
-- this needs to be modified for the endpoints. currently it crashes when it
-- tries to define them
--
-- each coefficient a_i is used over the interval [x_i, x_1+1]
interpolationArtifacts
    :: forall a. (Floating a, Ord a)
    => Boundary a -> Vector.Vector (a, a) -> [(a, a, Poly.Poly a)]
interpolationArtifacts b xs = [1 .. n - 1] <&> \i ->
    ( x i
    , x (i + 1)
    , Poly.Poly (x i) $ reverse
        [ (y' i + y' (i + 1) - 2 * s i) / (h i ^ 2)
        , (3 * s i - 2 * y' i - y' (i + 1)) / h i
        , y' i
        , y i ])
  where
    x, y :: Int -> a
    x i = xs & flip (Vector.!) i & fst
    y i = xs & flip (Vector.!) i & snd

    h, s, p, y' :: Int -> a
    h i = x (i + 1) - x i
    s i = (y (i + 1) - y i) / (x (i + 1) - x i)
    -- p is only used by y', so if y' is given p will never be called
    p i
        | i == 1    = boundary b P First
        | i == n    = boundary b P Last
        | otherwise = (s (i - 1) * h i + s i * h (i - 1)) / (h (i - 1) + h i)
    y' i
        | i == 1    = boundary b Y' First
        | i == n    = boundary b Y' Last
        | otherwise = (sign (s (i - 1)) + sign (s i)) * minimum
            [ abs (s (i - 1))
            , abs (s i)
            , 0.5 * abs (p i) ]

    sign b = if b >= 0 then 1 else (-1)

    n = Vector.length xs

    boundary :: Boundary a -> Var -> EndPoint -> a
    boundary (Known a _) Y' First = a
    boundary (Known _ a) Y' Last  = a
    boundary (Known _ _) _ _ = undefined

    boundary Parabola    P  First = s 1 * (1 + r) - s 2 * r
      where r = h 1 / (h 1 + h 2)
    boundary Parabola    P  Last  = s (n - 1) * (1 + r) - s (n - 2) * r
      where r = h (n - 1) / (h (n - 1) + h (n - 2))
    boundary Parabola    Y' First
        | p 1 * s 1 <= 0            = 0
        | abs (p 1) > 2 * abs (s 1) = 2 * s 1
        | otherwise                 = p 1
    boundary Parabola    Y' Last
        | p n * s (n - 1) <= 0            = 0
        | abs (p n) > 2 * abs (s (n - 1)) = 2 * s (n - 1)
        | otherwise                       = p n

    boundary NoCurve Y' First = 3 / 2 * s 1 - y' 2 / 2
    boundary NoCurve Y' Last  = 3 / 2 * s (n - 1) - y' (n - 2) / 2
    boundary NoCurve _  _     = undefined

    boundary (Exponential a _) Y' First = solveQuadratic
        (h 1)
        (4 * (a - y 1))
        ((a - y 1) * 2 * (3 * s 1 - y' 2))
        & filter (>= 0)
        & minimum
    boundary (Exponential _ b) Y' Last = solveQuadratic
        (h (n - 1))
        (2 * (b - y n))
        ((b - y n) * 2 * (3 * (s (n - 1)) - 2 * y' (n - 1)))
        & filter (>= 0)
        & minimum

-- these are internal to 'interpolationArtifacts', simply to make organizing
-- it a little cleaner
data EndPoint = First | Last
data Var = P | Y'

solveQuadratic :: forall a. (Floating a, Ord a) => a -> a -> a -> [a]
solveQuadratic a b c =
    [ (b' +- sqrt d) / a' | (+-) <- fns ]
  where
    b' = negate b
    a' = 2 * a
    d = b ^ 2 - 4 * a * c
    fns :: [a -> a -> a]
    fns = case compare d 0 of
        LT -> []
        EQ -> [const]
        GT -> [(+), (-)]

