
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures, BlockArguments #-}

module Stat where

import Prelude
import Control.Lens
import Data.List

instance (Num a, Num b) => Num (a, b) where
    (a0, a1) + (b0, b1) = (a0 + b0, a1 + b1)

expectedUnderIndependence :: forall a. (Fractional a) => [[a]] -> [[a]]
expectedUnderIndependence obs = iset (itraversed <.> itraversed) go obs
  where
    row, col :: [a]
    row = fmap sum obs
    col = fmap sum (transpose obs)

    n :: a
    n = sum row

    go :: (Int, Int) -> a
    go (r,c) =
        let r', c' :: a
            r' = row !! r
            c' = col !! c
        in r' * c' / n

testStatistics :: forall a. (Fractional a) => [[a]] -> [[a]]
testStatistics obs = (zipWith . zipWith) go obs (expectedUnderIndependence obs)
  where
    go o e = (o - e) ^ 2 / e

testStatistic :: forall a. (Floating a) => [[a]] -> a
testStatistic obs = sumOf (folded . folded) (testStatistics obs) -- goodnessOfFit $ zip (concat obs) (concat exp)
  --where
    --exp :: [[a]]
    --exp = expectedUnderIndependence obs

-- given a list of observed and expected
goodnessOfFit :: (Floating a) => [(a, a)] -> a
goodnessOfFit = sum . fmap go
  where
    go (o, e) = (o - e) ^ 2 / e

-- given a list of values and percentages
goodnessOfFit' :: forall a. (Floating a) => [(a, a)] -> a
goodnessOfFit' xs = xs
    & over (traverse . _2) (\p -> p / w * n)
    & goodnessOfFit
  where
    n, w :: a
    n = sumOf (traverse . _1) xs
    w = sumOf (traverse . _2) xs

example11'2 :: Bool
example11'2 = 3 == goodnessOfFit' [(15,0.2),(12,0.2),(9,0.2),(9,0.2),(15,0.2)]

hospital :: Double
hospital = goodnessOfFit
    [ (1524, 1494)
    , (104, 111)
    , (28, 25)
    , (44, 51)
    ]

vehicle :: Double
vehicle = goodnessOfFit'
    [ (206, 0.6)
    , (50, 0.2)
    , (30, 0.11)
    , (14, 0.09)
    ]

pets :: Double
pets = goodnessOfFit' $ correct
    [ (76, 30)  -- 0
    , (66, 30)  -- 1
    , (37, 20)  -- 2
    , (11, 10)  -- 3
    , (6, 5)    -- 4
    , (4, 5)    -- 5+
    ]
  where
    correct [a, b] = [a + b]
    correct (x : xs) = x : correct xs

vacation :: Double
vacation = goodnessOfFit' $ zip
    [40  , 53  , 50  , 34  , 37  , 194 , 741 , 410, 91  ,27   , 43  , 280]
    [0.02, 0.03, 0.03, 0.02, 0.02, 0.09, 0.37, 0.2, 0.05, 0.02, 0.02, 0.13]

shark = testStatistic
    [[ 502, 710, 659, 271]
    ,[ 300, 1482, 497, 229]]


test :: Double
test = testStatistic
    [[ 111 , 96 , 48 ]
    ,[ 96 , 133 , 61 ]
    ,[ 91 , 150 , 53 ]
    ]
