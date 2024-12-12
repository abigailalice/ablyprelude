
{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PackageImports, ExplicitNamespaces, MagicHash #-}

module AblyPrelude.Lens
    ( module X
    , lmapped
    , (++=)
    , foldMapOf#
    , observe
    , coerceOf
    , _Read
    , _IsList
    , type Sum, _Sum
    , type Product, _Product
    , type DList, _DList
    , type All, _All
    , type Any, _Any
    , type Ap, _Ap
    , type Alt, _Alt
    , _First, _First1
    , _Last, _Last1
    , _Min
    , _Max
    , (.#)
    , (#.)
    ) where

import qualified Data.Semigroup as DS
import Data.Monoid
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
import GHC.Exts as Exts hiding (Any)
import Data.DList as DList hiding (foldr)
import Data.Coerce as DC
import qualified Control.Lens as L

import AblyPrelude
import AblyPrelude.Lens.Strict as X

_IsList :: (IsList a, IsList b) => Iso a b [Exts.Item a] [Exts.Item b]
_IsList = iso Exts.toList Exts.fromList

(++=) :: (Writer.MonadWriter w m) => ASetter' w a -> a -> m ()
(++=) = scribe

foldMapOf#
    :: (Coercible r w, Monoid w)
    => Iso' r w -> Getting w s a -> (a -> r) -> s -> r
foldMapOf# c l f = review c #. foldMapOf l (view c #. f)

observe :: (State.MonadState s m) => LensLike' ((,) a) s a -> m a
observe l = State.state (l (\x -> (x, x)))

_Read :: (Show a, Read a, IsText t) => Prism' t a
_Read = _Text . _Show

-- * Wrapped isomorphisms
_Sum :: X.Iso (Sum a) (Sum b) a b
_Sum = X.coerced

_Product :: X.Iso (Product a) (Product b) a b
_Product = X.coerced

_Any :: X.Iso' Any Bool
_Any = X.coerced

_All :: X.Iso' All Bool
_All = X.coerced

_Endo :: X.Iso (Endo a) (Endo b) (a -> a) (b -> b)
_Endo = X.coerced

_Dual :: X.Iso (Dual a) (Dual b) a b
_Dual = X.coerced

_Alt :: X.Iso (Alt f a) (Alt g b) (f a) (g b)
_Alt = X.coerced

_First :: X.Iso (First a) (First b) (Maybe a) (Maybe b)
_First = X.coerced

_Last :: X.Iso (Last a) (Last b) (Maybe a) (Maybe b)
_Last = X.coerced

_First1 :: L.Iso (DS.First a) (DS.First b) a b
_First1 = L.coerced

_Last1 :: L.Iso (DS.Last a) (DS.Last b) a b
_Last1 = L.coerced

_Ap :: X.Iso (Ap f a) (Ap g b) (f a) (g b)
_Ap = X.coerced

_DList :: X.Iso (DList a) (DList b) [a] [b]
_DList = X.iso Exts.toList Exts.fromList -- X.iso DList.toList DList.fromList

_Min :: L.Iso (DS.Min a) (DS.Min b) a b
_Min = L.coerced

_Max :: Iso (DS.Max a) (DS.Max b) a b
_Max = L.coerced

infixr 9 #.
{-# INLINE (#.) #-}
(#.) :: (DC.Coercible b c) => (b -> c) -> (a -> b) -> a -> c
(#.) _ = DC.coerce

infixr 9 .#
{-# INLINE (.#) #-}
(.#) :: (DC.Coercible a b) => (b -> c) -> (a -> b) -> (a -> c)
(.#) f _ = DC.coerce f

lmapped :: Profunctor f => ASetter (f a r) (f b r) b a
lmapped f = Identity #. lmap (runIdentity #. f)

-- |@'coerceOf'@ is simply a slightly nicer replacement for calling 'coerce',
-- which avoids complex visible type applications or type signatures. Instead it
-- allows composition of isomorphisms to take the place of more complex type
-- signatures, 
--
-- @
-- coerceOf (_Sum . _Product) = coerce @a @(Sum (Product a))
-- @
--
-- Generally @coerceOf = review@ for isomorphisms which simply wrap/unwrap
-- newtypes.
coerceOf :: (DC.Coercible a b) => X.AnIso' a b -> b -> a
coerceOf _ = DC.coerce

