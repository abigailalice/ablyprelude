
{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Statistics
    ( Table(..), x0y0, x0y1, x1y0, x1y1, x0, x1, y0, y1, pX0, pX1, pY0, pY1
    , bootStrap
    , nullHypothesis
    , Freedom(..)
    , Options(..)

    ) where

import AblyPrelude
import AblyPrelude.Monad
import AblyPrelude.Lens
import qualified Control.Lens as Lens
import AblyPrelude.Data
import qualified Data.Map as Map

import qualified Pipes.Prelude as Pipes
import qualified AblyPrelude.Development as Development

-- x-axis = control vs. intervention = x-axis
-- y-axis = outcome
-- this is isomorphic to Bag ((X0 | X1) * (Y0 | Y1))
data Table a = Table
    { _x0y0 :: !a
    , _x0y1 :: !a
    , _x1y0 :: !a
    , _x1y1 :: !a
    } deriving (Foldable, Traversable, Functor, Show)
Lens.makeLenses ''Table
instance Applicative Table where
    pure a = Table a a a a
    Table a b c d <*> Table e f g h = Table (a e) (b f) (c g) (d h)
instance (Num a) => Num (Table a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger a = pure (fromInteger a)

-- projection functions
x0, x1, y0, y1 :: Num a => Table a -> a
x0 (Table {..}) = _x0y0 + _x0y1
x1 (Table {..}) = _x1y0 + _x1y1
y0 (Table {..}) = _x0y0 + _x1y0
y1 (Table {..}) = _x0y1 + _x1y1

pX0, pX1, pY0, pY1 :: (Num a, Integral a) => Table a -> Double
pX0 t = fromIntegral (x0 t) / fromIntegral (sum t)
pX1 t = fromIntegral (x1 t) / fromIntegral (sum t)
pY0 t = fromIntegral (y0 t) / fromIntegral (sum t)
pY1 t = fromIntegral (y1 t) / fromIntegral (sum t)

-- the difference in p(Y1|X0) and p(Y1|X1)
zScore :: (Num a, Integral a) => Table a -> Double
zScore t =
    let y1GivenX0 = fromIntegral (_x0y1 t) / fromIntegral (x0 t)
        y1GivenX1 = fromIntegral (_x1y1 t) / fromIntegral (x1 t)
    in y1GivenX0 - y1GivenX1

printTable :: (Show a, Num a) => Table a -> IO ()
printTable
    = Development.printAsTable
    . over (mapped . mapped) show
    . toLists
  where
    -- sequenceOf (traverse . traverse) :: [[b -> a]] -> b -> [[a]]
    toLists :: (Num a) => Table a -> [[a]]
    toLists = sequenceOf (traverse . traverse) 
        [ [_x0y0, _x0y1, x0 ]
        , [_x1y0, _x1y1, x1 ]
        , [y0,    y1   , sum]]


-- this could be done analytically with combinatoric functions, like
--
-- -- select a random subset of n items, and return the number selected, with
-- -- even probability
-- go :: Int -> m Int
-- go n = Control.Monad.Random.fromList $ fmap (\i -> (i, choose n i)) [1 .. n]
bootStrap :: forall m. (Applicative m, MonadRandom m) => Table Int -> m (Table Int)
bootStrap = traverse subSet
  where
    -- treating an integer 'm' as a set of 'm' elements, select a random subset,
    -- including or excluding each element with equal probability
    --
    -- note on the implementation, the same implementation with lists gives the
    -- same answer, but holds on to the list in memory until after after all
    -- random numbers have been generated. a more efficient manual recursive
    -- version is still trivial, but since i already have the pipes dependency
    -- i figure i should just use the effectful stream version, as it makes a
    -- lot of probability calculations nicer to work with
    subSet :: Int -> m Int
    subSet n = Pipes.sum (Pipes.replicateM n bit)
      where
        bit :: m Int
        bit = do
            b <- getRandom
            pure (if b then 1 else 0)

-- return true with a certain probability
probability :: (MonadRandom m, Random a, Num a, Ord a) => a -> m Bool
probability x = do
    p <- getRandomR (0, 1)
    pure (if p < x then True else False)




discreteCDF :: forall k. (Ord k, Num k, Fractional k) => Map k Int -> Map k k
discreteCDF = uncurry normalize . integrate
  where
    normalize z = fmap (/ z)

    integrate :: Map k Int -> (k, Map k k)
    integrate = Map.mapAccumWithKey go 0
      where
        go :: k -> k -> Int -> (k, k)
        go z k v = let z' = z + fromIntegral v * k in (z', z')



-- X0 v X1 = fixed
-- Y0 v Y1 = fixed
--
-- generates a new table, under the null hypothesis, without replacement. this
-- is appropriate when the number of X0vX1 is fixed, as is Y0vY1. this would not
-- be appropriate, for instance, for testing many hypotheses about drugs, as
-- there is no expectation that the number of people who could be successfully
-- treated for a disease is constant.
--
-- however, for statistics about promotions it is more appropriate, as the
-- number of promotions available is fixed, so it should never be the case that
-- every employee gets promoted, even in an unlikely scenario.
--
-- this might be able to be done analytically with combinatoric functions
--
-- it should be the case that the sums of the rows and columns are the same in
-- the original table and the simulated table
nullHypothesis_withoutReplacement
    :: forall m a. (MonadRandom m, Random a, Ord a, Integral a)
    => Table a -> m (Table a)
nullHypothesis_withoutReplacement ts = go x0 x1 y0 y1 0
  where
    -- randomly pick x value, assign it y1, until all y1s are exhausted
    -- only works if one x and one y value are non-zero
    -- it should be impossible for the xs and ys
    go :: a -> a -> a -> a -> Table a -> m (Table a)
    go 0 0 0 0 !z = pure z
    go x0 x1 y0 y1 z = do
        isX1 <- probability @m @Double (fromIntegral x1 / fromIntegral (x0 + x1))
        isY1 <- probability @m @Double (fromIntegral y1 / fromIntegral (y0 + y1))
        case (isX1, isY1) of
            (True , True ) -> go x0 (x1 - 1) y0 (y1 - 1) (z & x1y1 Lens.+~ 1)
            (True , False) -> go x0 (x1 - 1) (y0 - 1) y1 (z & x1y0 Lens.+~ 1)
            (False, True ) -> go (x0 - 1) x1 y0 (y1 - 1) (z & x0y1 Lens.+~ 1)
            (False, False) -> go (x0 - 1) x1 (y0 - 1) y1 (z & x0y0 Lens.+~ 1)
    x0, x1, y0, y1 :: a
    x0 = view x0y0 ts + view x0y1 ts
    x1 = view x1y0 ts + view x1y1 ts
    y0 = view x0y0 ts + view x1y0 ts
    y1 = view x0y1 ts + view x1y1 ts

-- X0 + X1 = fixed
-- Y0 + Y1 = unfixed
--
-- assumes the number of data points that get assigned to Y1 is unfixed. this is
-- appropriate for medical trials, as the number of people who will recover from
-- a treatment is able to vary from trial to trial.
--
-- By contrast, this may not work for a study on hirings for a position for a
-- job, as it may be assumed that the number of positions is fixed before the
-- hirings take place.
medicalTrial :: forall m. (MonadRandom m) => Int -> Int -> Double -> m (Table Int)
medicalTrial x0 x1 pY1 = do
    _x0y1 <- Pipes.sum (Pipes.replicateM x0 (prob pY1))
    _x1y1 <- Pipes.sum (Pipes.replicateM x1 (prob pY1))
    let _x0y0 = x0 - _x0y1
        _x1y0 = x1 - _x1y1
    pure $ Table {..}
  where
    prob :: Double -> m Int
    prob p = do
        x <- getRandom
        pure (if (x < p) then 1 else 0)

-- neither the control nor the response variable are allowed to vary. in other
-- words, the sum of each row and column are fixed, with any variation adhering
-- to this constraint
--
-- Fixed Control, Fixed Response
--
-- I could make these all a single function, with a type like
-- trial :: Either (Int, Int) Double -> Either (Int, Int) Double -> 
-- where the double represents the probability being constant (and thus the
-- number of results being free), while the pair of ints 
employmentTrial :: forall m. (MonadRandom m) => Table Int -> m (Table Int)
employmentTrial t = go (x0 t) (x1 t) (y0 t) (y1 t) 0
  where
    go 0 0 0 0 z = pure z
    go nX0 nX1 nY0 nY1 !z = do
        isX1 <- probability @m @Double $ nX1 // (nX1 + nX0)
        isY1 <- probability @m @Double $ nY1 // (nY1 + nY0)
        case (isX1, isY1) of
            (True, True)   -> go nX0 (pred nX1) nY0 (pred nY1) (z & x1y1 +~ 1)
            (True, False)  -> go nX0 (pred nX1) (pred nY0) nY1 (z & x1y0 +~ 1)
            (False, True)  -> go (pred nX0) nX1 nY0 (pred nY1) (z & x0y1 +~ 1)
            (False, False) -> go (pred nX0) nX1 (pred nY0) nY1 (z & x0y0 +~ 1)
    x // y = fromIntegral x / fromIntegral y


-- both the control and response variables are allowed to vary freely
--
-- Free Control, Free Response
observationalTrial :: MonadRandom m => Int -> Double -> Double -> m (Table Int)
observationalTrial n pX1 pY1 = Pipes.sum (Pipes.replicateM n go)
  where
    go = do 
        x1 <- probability pX1
        y1 <- probability pY1
        pure (case (x1, y1) of
            (True, True)   -> 0 & x1y1 .~ 1
            (True, False)  -> 0 & x1y0 .~ 1
            (False, True)  -> 0 & x0y1 .~ 1
            (False, False) -> 0 & x0y0 .~ 1)

data Freedom = ResponseFree | BothFree | NeitherFree

data Options = Options
    { _oneSided   :: Bool
    , _freedom    :: Freedom
    }

nullHypothesis :: MonadRandom m => Options -> Table Int -> m Bool
nullHypothesis (Options {..}) t = do
    t' <- simulate t
    pure $ asExtreme (zScore t) (zScore t')

  where
    -- checks that the magnitude of the test z-score is greater than the
    -- magnitude of the data z-score. if doing a one-sided test also checks the
    -- sign is the same, to confirm it isn't extreme but in a different
    -- direction
    asExtreme :: Double -> Double -> Bool
    asExtreme
        | _oneSided = \z z' -> (abs z' >= abs z) && (signum z == signum z')
        | otherwise = \z z' -> abs z' >= abs z
    simulate = case _freedom of
        BothFree     -> \t -> do
            t' <- bootStrap t
            observationalTrial (sum t) (pX1 t') (pY1 t')
        ResponseFree -> \t -> do
            t' <- bootStrap t
            medicalTrial (x0 t) (x1 t) (pY1 t')
        -- i'm not sure if it's possible to bootstrap this, as we need the same
        -- number of data points i thin
        NeitherFree  -> \t -> employmentTrial t





