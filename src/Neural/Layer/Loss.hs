
module Neural.Layer.Loss
    ( logLoss
    , expit, logit, softArgMax
    ) where

import AblyPrelude
import qualified Control.Lens as Lens

-- cross-entropy, as with softArgMax
logLoss :: (Foldable f, Floating a) => f (a, a) -> a
logLoss = negate . Lens.sumOf (Lens.folded . Lens.to \(y,yhat) -> y * log yhat)
--
-- 'expit' is the traditional sigmoidal function, and 'logit' is its inverse.
-- they have the domain (-inf,inf)->(0,1)
expit :: (Enum a, Floating a) => a -> a
expit = recip . succ . exp . negate

logit :: (Enum a, Floating a) => a -> a
logit = negate . log . subtract 1 . recip

-- |'softArgMax' is the vectorized generalization of 'expit', the sigmoid unit
softArgMax :: (Floating a, Functor f, Foldable f) => f a -> f a
softArgMax x = fmap (/ sum x') x'
  where
    x' = fmap exp x

