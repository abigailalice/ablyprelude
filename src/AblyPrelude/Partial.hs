
{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AblyPrelude.Partial.Error
    ( Partial(..)
    , runPartial
    ) where

import Prelude hiding (head)
import Data.Proxy (Proxy(..))
import Control.Exception
import Control.Exception.Safe (impureThrow)

import qualified Data.Reflection as Reflection

class (Exception e) => Partial s e | s -> e where
    partial :: proxy s -> a

instance (Reflection.Reifies s e, Exception e) => Partial s e where
    partial p = impureThrow (Reflection.reflect p)

partial_ :: forall s e a. (Partial s e) => a
partial_ = partial (Proxy :: Proxy s)

runPartial
    :: (Exception e)
    => e
    -> (forall s. (Partial s e) => Proxy s -> r)
    -> r
runPartial exc m = Reflection.reify exc m

head :: (Partial s e) => proxy s -> [a] -> a
head _ (x : _) = x
head p _ = partial p

example :: Int
example = runPartial exc (\p -> head p [1..])
  where
    exc :: PatternMatchFail
    exc = PatternMatchFail "AblyPrelude.Partial.Error.example has a bug, please report this"
