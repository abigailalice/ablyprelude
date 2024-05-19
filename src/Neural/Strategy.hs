
module Neural.Strategy
    ( exponentialMovingAverage
    , exponentialMovingAverage_Hat
    , rootMeanSquared_Hat

    , prev
    , delta
    , Neural.Strategy.sum
    , sparse
    , dynamicLearningRate

    , aggregatedMomentumDefault
    , traditionalMomentum
    , nesterovAcceleratedGradient
    , adagrad
    , adaptiveMomentEstimation
    , nesterovAcceleratedAdaptiveMomentEstimation

    , approxDiv
    , decide
    , Neural.Strategy.map
    ) where

import AblyPrelude hiding ((.), id)


import Control.Foldl as Fold hiding (std)
import Control.Category

data Mealy :: Type -> Type -> Type where
    Mealy :: s -> ((s, a) -> (s, b)) -> Mealy a b

instance Category Mealy where
    id = Mealy () id
    Mealy s0 g . Mealy r0 f = Mealy (s0, r0) \((s,r),x) ->
        let (r', y) = f (r, x)
            (s', z) = g (s, y)
        in ((s', r'), z)
instance Profunctor Mealy where
    dimap f g (Mealy s0 h) = Mealy s0 \(s, x) -> over _2 g $ h (s, f x)

instance Functor (Mealy a) where
    fmap f (Mealy s g) = Mealy s (over _2 f . g)
instance Applicative (Mealy a) where
    pure a = Mealy () \(s, _) -> (s, a)
    Mealy s0 f <*> Mealy r0 g = Mealy (s0, r0) \((s, r), x) ->
        let (s', y') = f (s, x)
            (r', z') = g (r, x)
        in ((s', r'), y' z')
instance (Num b) => Num (Mealy a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs    = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger x = pure (fromInteger x)
instance (Fractional b) => Fractional (Mealy a b) where
    (/) = liftA2 (/)
    fromRational x = pure (fromRational x)

exponentialMovingAverage :: (Num a) => a -> Mealy a a
exponentialMovingAverage beta = Mealy 0 \(momentum, x) ->
    let z = (alpha * x) + (beta * momentum)
    in (z, z)
  where
    alpha = 1 - beta

exponentialMovingAverage_Hat :: (Fractional a) => a -> Mealy a a
exponentialMovingAverage_Hat beta =
    liftA2 go (exponentialMovingAverage beta) (countFromZero @Int)
  where
    go m c = m / (1 - beta ^ c)

prev :: a -> Mealy a a
prev s0 = Mealy s0 swap
  where
    swap (a, b) = (b, a)

delta :: (Num a) => a -> Mealy a a
delta s0 = Mealy s0 (\(a, b) -> (b, b - a))

sum :: (Num a) => Mealy a a
sum = Mealy 0 (\(s, x) -> let !s' = s + x in (s', s'))

countFromZero :: (Num b) => Mealy a b
countFromZero = Mealy 0 (\(s, _) -> let !s' = s + 1 in (s', s'))



-- | Because the terminal velocity is 'recip (1 - beta)' under a constant
-- gradient, 'aggregatedMomentumDefault' 
--
-- Good default values are 3 and 0.1, which correspond to [0, 0.9, 0.99].
-- However, see the note under the 'aggregatedMomentum' regarding the use of
-- reparameterization.
aggregatedMomentumDefault :: (Fractional a) => Int -> a -> Mealy a a
aggregatedMomentumDefault n alpha = aggregatedMomentum betas
  where
    betas = [ 1 - alpha ^ (i - 1) | i <- [1 .. n] ]


traditionalMomentum :: (Fractional a) => a -> Mealy a a
traditionalMomentum beta = (\ema -> ema / (1 - beta))
    <$> exponentialMovingAverage beta

nesterovAcceleratedGradient :: (Fractional a) => a -> Mealy a a
nesterovAcceleratedGradient beta = ema + id
  where
    ema = fmap (\x -> beta / (1 - beta) * x) (exponentialMovingAverage beta)

aggregatedMomentum :: (Fractional a) => [a] -> Mealy a a
aggregatedMomentum betas = Fold.fold (Fold.premap go Fold.mean) betas + id
  where
    go beta = fmap (\x -> beta / (1 - beta) * x) (exponentialMovingAverage beta)

-- |'sparse' runs the underlying mealy machine only if given a non-zero
-- argument, otherwise it behaves as the identity.
--
-- As a motivating example see how 'sparse' behaves when used with
-- momentum-based mealy machines. Standard nesterov momentum can be implemented
-- like 'inertia decay + id', but if some 
--
-- sparse = divide (\x -> if x == 0 then Left x else Right x) id
sparse :: (Eq a, Num a) => Mealy a a -> Mealy a a
sparse (Mealy s f) = Mealy s go
  where
    go x = if snd x == 0 then (s, 0) else f x



-- |'dynamicLearningRate' multiplies a vector by a scalar, increasing or
-- decreasing the learning rate depending on the angle between the input and
-- output of the provided mealy machine.
--
-- I would like to come up with a formula that provides a dynamic learning rate
-- to a naive gradient function, but this doesn't do that (in any way that I
-- can see).
--
-- The most obvious implementation have the type 'a -> Mealy a a', and
-- have it compare the previous input with the current input, but at the cost
-- of storing the previous input in memory. Then simply putting it at the front
-- of the learning strategy gives a dynamic learning rate. I'm not sure how to
-- accomplish the same with the current implementation.
--
-- As for why I'm using this implementation, it works better with momentum
-- terms, for instance to adjust the learning rate for a momentum term by
-- comparing the angle between the gradient and the momentum, rather than the
--
-- This could be generalized as one of the following:
--
-- > compare 0 (zipWith (*) x y)
-- > lr * beta ^ (cos theta)
--
dynamicLearningRate :: (Fractional a, Ord a) => a -> Mealy a a -> Mealy a a
dynamicLearningRate beta (Mealy s0 f) = Mealy (1, s0) go
  where
    go ((lr, s), x) = ((lr', s'), y)
      where
        (s', y) = f (s, lr * x)
        lr' = case compare 0 (x * y) of
            LT -> lr / beta
            EQ -> lr
            GT -> lr * beta

adagrad :: (Floating a) => a -> Mealy a a
adagrad epsilon = id /~ rootMeanSquared
  where
    rootMeanSquared = dimap sqr sqrt Neural.Strategy.sum
    (/~) = liftA2 (approxDiv epsilon)
    sqr x = x ^ (2 :: Int)

approxDiv :: (Fractional a) => a -> a -> a -> a
approxDiv eps a b = a / (eps + abs b)

-- suggested default values are 1-10e-1, 1-10e-3, and 10e-8
--
-- The estimate for the variance is unnormalized, so substracting the mean
-- before 
adaptiveMomentEstimation :: (Floating a) => a -> a -> a -> Mealy a a
adaptiveMomentEstimation beta1 beta2 eps =
    exponentialMovingAverage_Hat beta1 / rootMeanSquared_Hat eps beta2

rootMeanSquared_Hat :: (Floating a) => a -> a -> Mealy a a
rootMeanSquared_Hat eps beta
    = dimap sqr ((+) eps . sqrt) (exponentialMovingAverage_Hat beta)
  where
    sqr x = x ^ (2 :: Int)

nesterovAcceleratedAdaptiveMomentEstimation :: (Fractional a, Floating a)
    => a -> a -> Mealy a a
nesterovAcceleratedAdaptiveMomentEstimation eps beta
    = nesterovAcceleratedGradient beta / std
  where
    std = rootMeanSquared_Hat eps beta
    

-- think this needs zip and unzip
map :: forall f a b. (Applicative f) => Mealy a b -> Mealy (f a) (f b)
map (Mealy s0 f) = Mealy (pure @f s0) \(s, x) ->
    let z = curry f <$> s <*> x
    in (fmap fst z, fmap snd z)

decide :: (a -> Either b c) -> Mealy b r -> Mealy c r -> Mealy a r
decide f (Mealy s0 g) (Mealy r0 h) = Mealy (s0, r0) \((s, r), x) -> case f x of
    Left a -> let (s', y) = g (s, a) in ((s', r), y)
    Right b -> let (r', y) = h (r, b) in ((s, r'), y)

