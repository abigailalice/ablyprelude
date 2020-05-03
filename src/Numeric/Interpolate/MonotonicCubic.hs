
module Numeric.Interpolate.MonotonicCubic
    (
    ) where

import AblyPrelude
import qualified Data.Vector as Vector

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
interpolationArtifacts
    :: forall a. (Fractional a, Ord a)
    => Vector.Vector (a, a) -> Vector.Vector (a, a, a, a)
interpolationArtifacts xs = Vector.zipWith4 (,,,) a b c d
  where
    a = Vector.generate n (\i -> (y' i + y' (i - 1) - 2 * s i) / (h i ^ 2))
    b = Vector.generate n (\i -> (3 * s i - 2 * y' i - y' (i - 1)) / h i)
    c = Vector.generate n (\i -> y' i)
    d = Vector.generate n (\i -> y i)

    x, y :: Int -> a
    x i = xs & flip (Vector.!) i & fst
    y i = xs & flip (Vector.!) i & snd

    h, s, p, y' :: Int -> a
    h i = x (i + 1) - x i
    s i = (y (i + 1) - y i) / (x (i + 1) - x i)
    p i = (s (i - 1) * h i + s i * h (i - 1)) / (h (i - 1) + h i)
    y' i = (sign (s (i - 1)) + sign (s i)) * minimum
        [ abs (s (i - 1))
        , abs (s i)
        , 0.5 * abs (p i) ]

    sign b = if b >= 0 then 1 else (-1)

    n = Vector.length xs

