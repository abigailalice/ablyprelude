
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
-- import Control.Exception
-- import Control.Exception.Safe (impureThrow)

import qualified GHC.Stack as GS

import qualified Data.Reflection as Reflection

class Partial_ s where
    partial :: GS.HasCallStack => proxy s -> a
instance (Reflection.Reifies s String) => Partial_ s where
    partial p = error (Reflection.reflect p)

type Partial s = (GS.HasCallStack, Partial_ s)

head :: (Partial s) => proxy s -> [a] -> a
head _ (x : _) = x
head p _ = partial p

last :: (Partial s) => proxy s -> [a] -> a
last p xs = foldl (flip const) (partial p) xs

fromMaybe :: Partial s => proxy s -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe p _ = partial p

-- unsafePreview :: (Partial s) => proxy s -> Getting (First a) s a -> s -> a
-- unsafePreview p l s = case preview l s of
--     Just a  -> a
--     Nothing -> partial p

runPartial
    :: String
    -> (forall s. (Partial_ s) => Proxy s -> r)
    -> r
runPartial msg m = Reflection.reify msg m
