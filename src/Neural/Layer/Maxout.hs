
module Neural.Layer.Maxout
    ( Bin(..), binaryFold, rotatingBinaryFold
    , maxOut, monotonicOut, convexOut, extremeOut, linearTransform
    , negateByIndex
    ) where

import AblyPrelude
import AblyPrelude.Partial
import Control.Lens (FunctorWithIndex(..))

import qualified Linear.Matrix as Linear
import qualified Linear.Vector as Linear

-- a balanced binary tree of functors
-- a nicer representation might be appending a 1 to the input vector, though
-- operationally this would need to be optimized away, and 
data P f g a = P (f (g a)) (f a) deriving (Functor, Foldable, Traversable)

data Bin a = Bin (Bin (a, a)) | Tip a deriving (Functor, Foldable, Traversable)

-- |'binaryFold' implements 'foldMap1' like funtionality for balanced binary
-- trees, except that the semigroup operation is passed as an argument rather
-- than type-specific
binaryFold :: (a -> b) -> (b -> b -> b) -> Bin a -> b
binaryFold g _ (Tip a) = g a
binaryFold g f (Bin a) = a & binaryFold g' f' & uncurry f
  where
     f' (p, q) (r, s) = (f p q, f r s)
     g' (p, q) = (g p, g q)

-- |'rotatingBinaryFold' behaves like 'binaryFold', except that a different
-- function is applied at each level of the tree, repeating once all the
-- functions 
rotatingBinaryFold :: (Partial s) => proxy s
    -> (a -> b) -> [b -> b -> b] -> Bin a -> b
rotatingBinaryFold _ g _  (Tip a) = g a 
rotatingBinaryFold p g fs (Bin a) = a & rotatingBinaryFold p g' fs'
    & uncurry (AblyPrelude.Partial.head p fs)
  where
    fs' = rotate $ map (\f (s, t) (u, v) -> (f s t, f u v)) fs
    g' (s, t) = (g s, g t)
    rotate :: [c] -> [c]
    rotate xs = cycle xs & tail & zipWith (flip const) xs

linearTransform
    :: (Applicative o, Functor d, Foldable i, Linear.Additive i, Linear.Additive d, Num a)
    => P o i a
    -> i (d a)
    -> o (d a)
linearTransform (P w b) x = b !+! (w Linear.!*! x)
  where
    (!+!) = liftA2 (\t -> fmap (+ t))

-- mapOut :: (Functor f, Applicative o, Linear.Additive i, Linear.Additive d, Num a, Foldable i
--     , Traversable f, Applicative d)
--     => (f a -> a) -> f (P o i a) -> i (d a) -> o (d a)
-- mapOut f w x = fmap (linearTransform Lens.?? x) w & sequenceA & fmap sequenceA
--     & fmap (fmap f)

-- | As a single layer of 'maxOut' defines a convex function, 'negateMaxOut'
-- defines functions with a single extreme point (and so allows for inflection
-- points). If the values of the W matrix are restricted to be positive only
-- then 'negateMaxOut' defines monotonic functions
maxOut, monotonicOut, convexOut, extremeOut
    :: (Num a, Ord a, Applicative o, Foldable i, Linear.Additive i
    , Linear.Additive d, Linear.Additive o, Applicative d, FunctorWithIndex Integer o)
    => Bin (P o i a)
    -> i (d a)
    -> o (d a)
maxOut p x = binaryFold (\w -> linearTransform w x) (liftA2 (liftA2 max)) p
monotonicOut p x 
    = binaryFold (\w -> linearTransform w x) (liftA2 (liftA2 negMax)) p
  where
    negMax a b = negate (max a b)
convexOut p x  = negateByIndex (maxOut p x)
extremeOut p x = negateByIndex (monotonicOut p x)


-- With positive weight matrices we have the following:
--
-- Convex functions:
--     negateByIndex . maxOut
-- Functions with no more than 1 extreme point:
--     negateByIndex . negateMaxOut
--
-- The fact that liftU2 ignores zeros makes the nand network poorly behaved,
-- and so in that case i think we need to alternate max/min
negateByIndex
    :: (FunctorWithIndex i m, Functor d, Integral i, Num a)
    => m (d a) -> m (d a)
negateByIndex = imap (\i k -> if even i then k else fmap negate k)

