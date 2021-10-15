
{-# LANGUAGE ExplicitNamespaces #-}

module AblyPrelude.Functor
    ( module X
    , Fix(..)
    ) where

import Data.Functor.Compose as X
import Data.Functor.Identity as X
import Data.Functor.Const as X
import GHC.Generics as X ((:*:)(..), (:+:)(..), V1, U1(..))

newtype Fix f = Fix { runFix :: f (Fix f) }

