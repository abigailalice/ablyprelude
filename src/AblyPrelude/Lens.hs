
{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PackageImports, ExplicitNamespaces, MagicHash #-}

module AblyPrelude.Lens
    ( module X
    , (++=)
    , foldMapOf#
    ) where

import "lens" Control.Lens as X
    ( Field1(..), Field2(..), Field3(..), Field4(..), Field5(..), Field6(..)
    , Field7(..), Field8(..), Field9(..), Field10(..), Field11(..), Field12(..)
    , Each(..), Ixed(..), At(..), Contains(..), Strict(..), lazy
    , type Lens, type Lens', type Prism, type Prism', type Iso, type Iso'
    , type Traversal, type Traversal'
    , under
    , view, toListOf, preview, over, set, use, preuse, forOf, forOf_
    , (^.), (^..), (^?), (%~), (.~)
    , iview, toListOf, ipreview, iover, iset, iuse, ipreuse, iforOf, iforOf_
    , (.=)
    -- lenses
    , foldMapOf
    , folded

    -- prisms
    , _Left, _Right, _Show, _Just, _Nothing

    -- isos
    , IxValue, Index
    )
import "generic-lens" Data.Generics.Product.Any as X (HasAny(..))
import "generic-lens" Data.Generics.Product.Typed as X (HasType(..))
import "generic-lens" Data.Generics.Product.Types as X (HasTypes)
import "generic-lens" Data.Generics.Product.Constraints as X (HasConstraints(..), HasConstraints'(..))
import "generic-lens" Data.Generics.Product.Param as X (HasParam(..))
import "generic-lens" Data.Generics.Product.Positions as X (HasPosition(..))
import Control.Lens hiding (set')
import Control.Monad.Writer

import AblyPrelude.Lens.Strict as X
import AblyPrelude

(++=) :: (MonadWriter w m) => ASetter' w a -> a -> m ()
(++=) = scribe

foldMapOf#
    :: (Coercible r w, Monoid w)
    => Iso' r w -> Getting w s a -> (a -> r) -> s -> r
foldMapOf# c l f = view (from c) #. foldMapOf l (view c #. f)


