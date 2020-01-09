
module AblyPrelude.Functor
    ( module X
    , Fix(..)
    ) where

import Data.Functor.Compose as X
import Data.Functor.Product as X
import Data.Functor.Sum as X
import Data.Functor.Const as X

newtype Fix f = Fix { runFix :: f (Fix f) }

