
{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ConstraintKinds #-}

module AblyPrelude.Partial
    ( Partial
    , partial
    , runPartial
    , head
    , fromJust
    ) where

-- import Data.List.NonEmpty hiding (head)
-- import Data.Text (Text)
import Prelude hiding (head)
import qualified Prelude
import Data.Proxy (Proxy(..))

import  GHC.Exts 
import qualified Control.Exception as CE
import qualified GHC.Stack as GS

import qualified Data.Reflection as Reflection

-- |@'Partial_'@ is a class for partial values. It would be better if we could
-- write this as @class HasCallStack => Partial s@, because we want to make it
-- impossible for users to fail to extend the call stack when calling a partial
-- function, but due to the way GHC implements @HasCallStack@ (as an implicit
-- parameter) disallows it appearing in class heads. To get around this we
-- export a @Partial@ type constraint instead, which includes @Partial_@, but
-- also @HasCallStack@.
--
-- This ensures that the call stack is preserved between @partial@ and
-- @runPartial@.
class Partial_ s where
    partial :: GS.HasCallStack => proxy s -> a
instance (Reflection.Reifies s PartialException) => Partial_ s where
    partial p
        = CE.throw (PartialException (cleanupCallStack caller GS.callStack) msg)
      where
        PartialException callStack msg = Reflection.reflect p
        caller = Prelude.head (toList callStack)

type Partial s = (GS.HasCallStack, Partial_ s)

-- |@'PartialException'@ is used to manually remove the first and last elements
-- from the call stack when it gets printed to the terminal, removing the call
-- to @partial@ and @runPartial@, which only leaving only the function which
-- actually _called_ @partial@, and its callers. This avoids any of the
-- functions from this module appearing in stack traces.
--
-- It's ironic that the show instance for this uses partial functions unsafely.
data PartialException = PartialException GS.CallStack String
    deriving CE.Exception
instance Show PartialException where
    show (PartialException callStack msg) = "PartialException\n"
        <> GS.prettyCallStack callStack <> "\n" <> msg

cleanupCallStack :: ([Char], GS.SrcLoc) -> GS.CallStack -> GS.CallStack
cleanupCallStack caller
    = fromList . reverse . tail . (:) caller . reverse . tail . toList

head :: (Partial s) => proxy s -> [a] -> a
head _ (x : _) = x
head p _ = partial p

-- tail :: (Partial s) => proxy s -> [a] -> [a]
-- tail _ (_ : xs) = xs
-- tail p _ = partial p

last :: (Partial s) => proxy s -> [a] -> a
last p xs = foldl (flip const) (partial p) xs

nested :: (Partial s) => proxy s -> [a] -> a
nested p = head p

test :: Int
test = runPartial "test" (\p -> nested p [])

test2 :: GS.HasCallStack => Int
test2 = test

fromJust :: Partial s => proxy s -> Maybe a -> a
fromJust _ (Just x) = x
fromJust p _ = partial p

runPartial :: GS.HasCallStack => String -> (forall s. (Partial s) => Proxy s -> r) -> r
runPartial msg = Reflection.reify (PartialException GS.callStack msg)
