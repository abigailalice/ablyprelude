
{-# LANGUAGE PackageImports #-}

module AblyPrelude
    ( module X
    , fmap_
    , bind
    , (.#)
    , (#.)
    , nfIO
    , whnfIO
    , undefined
    ) where

import "base" Data.String as X (IsString(..))
import "base" Data.Function as X ((&))
import "base" Data.Functor as X ((<&>), (<$), ($>))
import "safe-exceptions" Control.Exception.Safe as X
    ( MonadThrow
    , MonadCatch
    , MonadMask(..)
    , mask_
    , throw
    , catch
    , handle
    , try
    , bracket
    , bracket_
    , finally )
import qualified Control.Exception as Exception
import Control.Applicative as X
import Control.Monad as X
import Data.Monoid as X
import Prelude as X hiding (undefined)
import Data.Text as X (Text)
import Data.Foldable as X
import Data.Maybe as X (isJust)

import Data.Kind as X (Type)
import Control.DeepSeq as X (NFData(..), force, deepseq)
import Data.Coerce as X (Coercible, coerce)
import Data.Proxy as X (Proxy(..))
import System.IO.Unsafe as X (unsafePerformIO)

import qualified Prelude as P

{-# INLINE fmap_ #-}
fmap_ :: (Functor f) => a -> f unit -> f a
fmap_ = (<$)

bind :: (Monad m) => (a -> m b) -> m a -> m b
bind = (=<<)

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = P.undefined

-- trace

infixr 9 #.
{-# INLINE (#.) #-}
(#.) :: (X.Coercible b c) => (b -> c) -> (a -> b) -> a -> c
(#.) _ = X.coerce

infixr 9 .#
{-# INLINE (.#) #-}
(.#) :: (X.Coercible a b) => (b -> c) -> (a -> b) -> (a -> c)
(.#) f _ = X.coerce f

{-# INLINE nfIO #-}
nfIO :: (X.NFData a) => a -> IO a
nfIO = Exception.evaluate . X.force

{-# INLINE whnfIO #-}
whnfIO :: a -> IO a
whnfIO = Exception.evaluate

