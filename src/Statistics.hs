
{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Statistics
    ( Table(..), x0y0, x0y1, x1y0, x1y1, x0, x1, y0, y1, pX0, pX1, pY0, pY1
    , bootStrap
    , nullHypothesis
    , Freedom(..)
    , Options(..)
    -- , printTable

    , discreteCDF
    , nullHypothesisN
    ) where

import AblyPrelude
import AblyPrelude.Monad
import AblyPrelude.Lens
import qualified Control.Lens as Lens
import AblyPrelude.Data
import qualified Data.Map as Map

import qualified Pipes.Prelude as Pipes
import qualified Pipes
import qualified Control.Foldl as Foldl
-- import qualified AblyPrelude.Development as Development

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
-- the difference between the 
zScore :: (Num a, Integral a) => Table a -> Double
zScore t =
    let y1GivenX0 = fromIntegral (_x0y1 t) / fromIntegral (x0 t)
        y1GivenX1 = fromIntegral (_x1y1 t) / fromIntegral (x1 t)
    in y1GivenX1 - y1GivenX0

-- printTable :: forall a. (Show a, Num a) => Table a -> IO ()
-- printTable
--     = Development.printAsTable
--     . toLists
--   where
--     -- sequenceOf (traverse . traverse) :: [[b -> a]] -> b -> [[a]]
--     toLists :: (Show a) => Table a -> [[Text]]
--     toLists t =
--         [ [""  , "Y0"   , "Y1"   , ""   ]
--         , ["X0", show $ _x0y0 t, show $ _x0y1 t, show $ x0 t ]
--         , ["X1", show $ _x1y0 t, show $ _x1y1 t, show $ x1 t ]
--         , [""  , show $ y0 t   , show $ y1 t, show $ sum t]]


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




discreteCDF
    :: forall k. (Ord k, Num k, Fractional k)
    => [k] -> [(k, k)]
discreteCDF
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
    integrate = Map.mapAccumWithKey go 0
      where
        -- it may also be reasonable to return (z', z), which returns the LT
        -- percentiles rather than LTE. some average of the two may also be
        -- sensible
        go :: k -> k -> Int -> (k, k)
        go z k v = let z' = z + fromIntegral v * k in (z', z')




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

nullHypothesisN :: forall m. MonadRandom m => Int -> Options -> Table Int -> m Double
nullHypothesisN n o t = average (Pipes.replicateM n oneSample)
  where
    average :: Pipes.Producer Double m () -> m Double
    average = Foldl.purely Pipes.fold Foldl.mean
    oneSample :: m Double
    oneSample = do
        b <- nullHypothesis o t
        pure (if b then 1 else 0)

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
            t' <- pure t -- bootStrap t
            observationalTrial (sum t) (pX1 t') (pY1 t')
        ResponseFree -> \t -> do
            t' <- pure t -- bootStrap t
            medicalTrial (x0 t) (x1 t) (pY1 t')
        -- i'm not sure if it's possible to bootstrap this, as we need the same
        -- number of data points i thin
        NeitherFree  -> \t -> employmentTrial t





