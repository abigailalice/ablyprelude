
{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PackageImports, ExplicitNamespaces #-}

module AblyPrelude.Lens
    ( module X
    , (++=)
    ) where

import "lens" Control.Lens as X
    ( Field1(..), Field2(..), Field3(..), Field4(..), Field5(..), Field6(..), Field7(..), Field8(..)
    , Each(..), Ixed(..), At(..), Contains(..), Strict(..), lazy
    , type Lens, type Lens', type Prism, type Prism', type Iso, type Iso'
    , under
    , view, toListOf, preview, over, set, forOf, forOf_
    , iview, toListOf, ipreview, iover, iset, iforOf, iforOf_
    -- lenses

    -- prisms
    , _Left, _Right, _Show, _Just, _Nothing

    -- isos
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

(++=) :: (MonadWriter w m) => ASetter' w a -> a -> m ()
(++=) = scribe
