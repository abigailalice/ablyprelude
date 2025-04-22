
module Data.Maybe.Church
    ( Maybe(..)
    ) where

import Prelude hiding (Maybe)
import Control.Monad
import Control.Lens

newtype Maybe a = Maybe { unMaybe :: forall r. r -> (a -> r) -> r }

instance Functor Maybe where
    fmap f (Maybe m) = Maybe \nothing just -> m nothing (just . f)

instance Applicative Maybe where
    pure a = Maybe (\_ just -> just a)
    -- Maybe f <*> Maybe x = Maybe (\n j -> let f' = f n
    --                                          x' = x n
    --                             in f' & argument . argument %~ _
    --                                   -- -- & argument %~ _
    --                                   -- & argument . result %~ (\g -> _ )
    --                                   & _)
    (<*>) = ap
    -- Maybe f <*> Maybe x = Maybe \n j -> f n (_)

instance Monad Maybe where
    Maybe m >>= f = Maybe \n j -> m n
        & argument . argument %~ (($ j) . ($ n) . unMaybe . f)
        & ($ id)


