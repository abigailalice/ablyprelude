
{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ConstraintKinds #-}

module AblyPrelude.Partial
    ( Partial
    , partial
    , runPartial
    , head
    , fromMaybe
    ) where

-- import Data.List.NonEmpty hiding (head)
-- import Data.Text (Text)
import Prelude hiding (head)
import Data.Proxy (Proxy(..))

import qualified Control.Exception as CE
import qualified GHC.Stack as GS

import qualified Data.Reflection as Reflection

-- |@'Partial_'@ is a class for partial values. It would be better if we could
-- write this as @class HasCallStack => Partial s@, but due to the way GHC
-- implements @HasCallStack@ (as an implicit parameter) disallows it appearing
-- in class heads. As we want @Partial@ functions to always extend the call
-- stack we use a @Partial@ type constraint, and export that instead, so that
-- users cannot fail to include the @HasCallStack@ constraint by mistake.
--
class Partial_ s where
    partial :: GS.HasCallStack => proxy s -> a
instance (Reflection.Reifies s String) => Partial_ s where
    partial p = CE.throw (PartialException GS.callStack (Reflection.reflect p))

type Partial s = (GS.HasCallStack, Partial_ s)

-- |@'PartialException'@ is used to manually remove the first and last elements
-- from the call stack, when it gets printed to the terminal, removing the call
-- to @partial@ and @runPartial@, which only leaving only the function which
-- actually _called_ @partial@, and its callers.
--
-- It's ironic that the show instance for this uses partial functions unsafely.
data PartialException = PartialException GS.CallStack String
    deriving CE.Exception
instance Show PartialException where
    show (PartialException n m) = GS.prettyCallStack callStack <> "\n" <> m
      where
        callStack = foldr GS.pushCallStack GS.emptyCallStack
            $ reverse $ tail $ reverse $ tail
            $ GS.getCallStack $ n

head :: (Partial s) => proxy s -> [a] -> a
head _ (x : _) = x
head p _ = partial p

last :: (Partial s) => proxy s -> [a] -> a
last p xs = foldl (flip const) (partial p) xs

fromMaybe :: Partial s => proxy s -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe p _ = partial p

runPartial :: String -> (forall s. (Partial s) => Proxy s -> r) -> r
runPartial msg m = Reflection.reify msg m
