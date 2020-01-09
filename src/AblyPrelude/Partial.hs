
{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AblyPrelude.Partial
    ( Partial(..)
    , runPartial
    ) where

import Data.List.NonEmpty
import Data.Text (Text)
import Prelude hiding (head)
import Data.Proxy (Proxy(..))
import Control.Exception
import Control.Exception.Safe (impureThrow)

import qualified Data.Reflection as Reflection

class Partial s where
    partial :: proxy s -> a
instance (Reflection.Reifies s TraceBack) => Partial s where
    partial p = impureThrow (Reflection.reflect p)

newtype TraceBack = TraceBack (NonEmpty Text) deriving (Show)
_TraceBack :: (Functor f)
    => (NonEmpty Text -> f (NonEmpty Text)) 
    -> TraceBack -> f TraceBack
_TraceBack f (TraceBack x) = TraceBack <$> f x
instance Exception TraceBack

-- If viewing the type
--   (Partial s => r) ~ (NonEmpty Text -> r)
-- then stackFrame is simply
--   stackFrame msg = local (msg :)
{-
stackFrame :: Text
    -> (Partial s => Proxy s -> r)
    -> (Partial s1 => proxy s1 -> r)
stackFrame msg f p
    = Reflection.reify (over _TraceBack (Data.List.NonEmpty.cons msg) (Reflection.reflect p)) f
error :: (Partial s) => Text -> proxy s -> r
error msg p = stackFrame msg partial p
-}

runPartial
    :: Text
    -> (forall s. (Partial s) => Proxy s -> r)
    -> r
runPartial msg m = Reflection.reify (TraceBack (pure msg)) m

