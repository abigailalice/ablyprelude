
{-# LANGUAGE PackageImports, PatternSynonyms, TypeOperators #-}

module AblyPrelude
    ( module X
    , fmap_
    , filterMap
    , bind
    , (.#)
    , (#.)
    , nfIO
    , whnfIO
    , errorIO
    , show
    , pattern (:=>), type (:=>)
    --, type List1
    , coerceOf
    , coerceFromOf
    ) where


import qualified Prelude as Prelude
import qualified Data.Text.Lens as Lens
import qualified Control.Lens as Lens

import GHC.Generics as X (Generic)

import Data.Functor.Contravariant as X (Contravariant(..))
import Data.Bifunctor as X (Bifunctor(..))
import Data.Semigroup.Foldable as X (Foldable1(..))
import GHC.Exts as X (IsList)
import Data.Function as X ((&))
import Data.Functor.Identity as X (Identity(..))
import Data.Functor as X ((<&>), ($>))
import Data.Foldable as X
import Data.Maybe as X (isJust)
import Data.Monoid as X
import Data.String as X (IsString(..))
import Data.Text as X (Text)
import Data.Text.IO as X (putStr, putStrLn)
import Data.Void as X (Void, absurd)
import Control.Applicative as X
import Control.Monad as X
import Prelude as X hiding (undefined, putStr, putStrLn, show, String, read)

import Data.Kind as X (Type)
import Control.DeepSeq as X (NFData(..), force, deepseq)
import Data.Coerce as X (Coercible, coerce)
import Data.Proxy as X (Proxy(..))
import System.IO.Unsafe as X (unsafePerformIO)

import Data.Generics.Product.Fields as X (HasField, HasField')

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
import qualified GHC.Stack as GS
import GHC.Stack as X (HasCallStack)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x : xs) = case f x of
    Just x' -> x' : filterMap f xs
    Nothing -> filterMap f xs

type (:=>) a b = (a, b)
pattern (:=>) :: a -> b -> (a, b)
pattern a :=> b = (a, b)

{-# INLINE fmap_ #-}
fmap_ :: (Functor f) => a -> f unit -> f a
fmap_ = (<$)

{-# INLINE bind #-}
bind :: (Monad m) => (a -> m b) -> m a -> m b
bind = (=<<)

show :: (Show a, Lens.IsText b) => a -> b
show = Lens.view Lens.packed . Prelude.show


-- type List1 a = NonEmpty a


-- trace

infixr 9 #.
{-# INLINE (#.) #-}
(#.) :: (X.Coercible b c) => (b -> c) -> (a -> b) -> a -> c
(#.) _ = X.coerce

infixr 9 .#
{-# INLINE (.#) #-}
(.#) :: (X.Coercible a b) => (b -> c) -> (a -> b) -> (a -> c)
(.#) f _ = X.coerce f

_Sum :: Lens.Iso (Sum a) (Sum b) a b
_Sum = Lens.iso getSum Sum

-- |@'coerceFromOf'@ is simply a slightly nicer replacement for calling
-- 'coerce', which avoids complex visible type applications or type signatures
coerceFromOf :: (Coercible a b) => Lens.AnIso' a b -> a -> b
coerceFromOf _ = coerce

coerceOf :: (X.Coercible a b) => Lens.AnIso' a b -> b -> a
coerceOf _ = coerce

{-# INLINE nfIO #-}
nfIO :: (X.NFData a) => a -> IO a
nfIO = Exception.evaluate . X.force

{-# INLINE whnfIO #-}
whnfIO :: a -> IO a
whnfIO = Exception.evaluate

{-# INLINE errorIO #-}
errorIO :: GS.HasCallStack => [Char] -> IO a
errorIO = whnfIO . error

