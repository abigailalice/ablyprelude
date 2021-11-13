
{-# LANGUAGE PackageImports, PatternSynonyms, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeFamilies, ViewPatterns, FlexibleInstances #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}

module AblyPrelude
    ( module X
    , fmap_
    , filterMap
    , bind
    , nf
    , nfIO
    , whnfIO
    , errorIO
    , show
    , pattern (:=>), type (:=>)
    --, type List1
    ) where


import qualified Prelude as Prelude
import qualified Data.Text.Lens as Lens
import qualified Control.Lens as Lens

import GHC.Generics as X (Generic)

import AblyPrelude.Partial as X
import Control.Monad.IO.Class as X (MonadIO(..))
import Data.Functor.Contravariant as X (Contravariant(..))
import Data.Bifunctor as X (Bifunctor(..))
import Data.Profunctor as X (Profunctor(..))
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
import Prelude as X hiding (undefined, putStr, putStrLn, show, String, read, head)

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

filterMap :: forall a b. (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr go []
  where
    go :: a -> [b] -> [b]
    go x ys = case f x of
        Just y -> y : ys
        Nothing -> ys

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

{-# INLINE nfIO #-}
nfIO :: (X.NFData a) => a -> IO a
nfIO = Exception.evaluate . X.force

{-# INLINE nf #-}
nf :: (X.NFData a) => a -> a
nf = X.force

{-# INLINE whnfIO #-}
whnfIO :: a -> IO a
whnfIO = Exception.evaluate

{-# INLINE errorIO #-}
errorIO :: GS.HasCallStack => [Char] -> IO a
errorIO = liftIO . whnfIO . error

