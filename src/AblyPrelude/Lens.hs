
{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PackageImports, ExplicitNamespaces, MagicHash #-}

module AblyPrelude.Lens
    ( module X
    , (++=)
    , foldMapOf#
    , observe
    , _Read
    , _IsList
    ) where

import Control.Lens as X hiding (set')
import Data.Generics.Product.Any as X (HasAny(..))
import Data.Generics.Product.Typed as X (HasType(..))
import Data.Generics.Product.Types as X (HasTypes)
import Data.Generics.Labels ()
-- import "generic-lens" Data.Generics.Product.Constraints as X (HasConstraints(..), HasConstraints'(..))
import Data.Generics.Product.Param as X (HasParam(..))
import Data.Generics.Product.Positions as X (HasPosition(..))
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.State as State
import Data.Text.Lens as X (_Text, IsText)
import GHC.Exts as Exts

import AblyPrelude
import AblyPrelude.Lens.Strict as X

_IsList :: (IsList a) => Iso' a [Exts.Item a]
_IsList = iso Exts.toList Exts.fromList

(++=) :: (Writer.MonadWriter w m) => ASetter' w a -> a -> m ()
(++=) = scribe

foldMapOf#
    :: (Coercible r w, Monoid w)
    => Iso' r w -> Getting w s a -> (a -> r) -> s -> r
foldMapOf# c l f = view (from c) #. foldMapOf l (view c #. f)

observe :: (State.MonadState s m) => LensLike' ((,) a) s a -> m a
observe l = State.state (l (\x -> (x, x)))

_Read :: (Show a, Read a, IsText t) => Prism' t a
_Read = _Text . _Show

