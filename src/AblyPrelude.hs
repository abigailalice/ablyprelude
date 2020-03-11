
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
    --, type List1
    ) where

-- import Data.List.NonEmpty
import Data.Function as X ((&))
import Data.Functor as X ((<&>), (<$), ($>))
import Data.Foldable as X
import Data.Maybe as X (isJust)
import Data.Monoid as X
import Data.String as X (IsString(..))
import Data.Text as X (Text)
import Data.Void as X (Void, absurd)
import Control.Applicative as X
import Control.Monad as X
import Prelude as X hiding (undefined)

import Data.Kind as X (Type)
import Control.DeepSeq as X (NFData(..), force, deepseq)
import Data.Coerce as X (Coercible, coerce)
import Data.Proxy as X (Proxy(..))
import System.IO.Unsafe as X (unsafePerformIO)

import qualified Prelude as P


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


{-# INLINE fmap_ #-}
fmap_ :: (Functor f) => a -> f unit -> f a
fmap_ = (<$)

{-# INLINE bind #-}
bind :: (Monad m) => (a -> m b) -> m a -> m b
bind = (=<<)

-- type List1 a = NonEmpty a

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

