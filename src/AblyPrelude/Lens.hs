
{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PackageImports, ExplicitNamespaces, MagicHash #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

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
    , _Pure
    , scanlOf
    , reindexedBy
    , ifilteredBy
    , ifolding'
    , ito'
    , foldingOf
    , scanOver
    , indented
    , indent
    , intercalatedBy
    , intercalated
    , lines
    ) where

import qualified Data.Semigroup as DS
import Data.Monoid
import Control.Lens as X hiding (set')
import qualified Control.Monad.State.Strict as CMSS
import Data.Generics.Product.Any as X (HasAny(..))
import Data.Generics.Product.Typed as X (HasType(..))
import Data.Generics.Product.Types as X (HasTypes)
import Data.Generics.Labels ()
-- import "generic-lens" Data.Generics.Product.Constraints as X (HasConstraints(..), HasConstraints'(..))
import Data.Generics.Product.Param as X (HasParam(..))
import Data.Generics.Product.Positions as X (HasPosition(..))
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.State as State
import qualified Data.Text as DT
import Data.Text.Lens as X (_Text, IsText)
import GHC.Exts as Exts hiding (Any)
import Data.DList as DList hiding (foldr)
import Data.Coerce as DC
import qualified Control.Lens as L
import qualified Control.Foldl as CF
import qualified Control.Scanl as CS

import AblyPrelude hiding (lines)
import AblyPrelude.Lens.Strict as X

-- {{{ tedious type wrapping
_IsList :: (IsList a, IsList b) => Iso a b [Exts.Item a] [Exts.Item b]
_IsList = iso Exts.toList Exts.fromList

_Read :: (Show a, Read a, IsText t) => Prism' t a
_Read = _Text . _Show
-- }}}

(++=) :: (Writer.MonadWriter w m) => ASetter' w a -> a -> m ()
(++=) = scribe


-- {{{ indexed
-- |Uses a filter to obtain a new index, while still passing the old index
-- through.
--
-- Fold (k, a) i -> Optical' p_k p_i a (k, a)
filteredByWithIndex
    :: (Indexable i p, Applicative f)
    => Fold (k, a) i -> Optical' p (Indexed k) f a (k, a)
filteredByWithIndex f = withIndex .> filteredBy f

-- iifolding :: ((i, s) -> f 
-- ifolding  :: (s -> f (i, a)) -> Optical' p   p_i s a
-- ifolding' :: (i -> s -> f a) -> Optical' p_i p_i s a
ifolding' :: (Foldable t, Applicative f, Contravariant f, Indexable i p)
    => (i -> s -> t a) -> Optical' p (Indexed i) f s a
ifolding' f = withIndex <. folding (\(i, s) -> f i s)

-- ifoldingWithIndex
--     :: (Foldable t, Applicative f, Contravariant f, Indexable k p)
--     => (i -> s -> t (k, a)) -> Optical' p (Indexed i) f s a
-- ifoldingWithIndex f = withIndex .> ifolding (\(i, s) -> f i s)

-- itoWithIndex
--     :: (Applicative f, Contravariant f, Indexable k p)
--     => (i -> s -> (k, a)) -> Optical' p (Indexed i) f s a
-- itoWithIndex f = withIndex .> ito (\(i, s) -> f i s)


-- ito  :: (s -> (i, a)) -> Optical' p   p_i s a
-- ito' :: (i -> s -> a) -> Optical' p_i p_i s a
ito' :: (Indexable i p, Contravariant f, Functor f)
    => (i -> s -> a) -> Optical' p (Indexed i) f s a
ito' f = withIndex <. to (\(i, s) -> f i s)

-- Fold k i -> Optical' p_k p_i a a
ifilteredBy :: (Indexable i p, Applicative f)
    => Fold k i -> Optical' p (Indexed k) f a a
ifilteredBy f = filteredByWithIndex (_1 . f) <. _2

reindexedBy :: Fold k i -> IndexedTraversal' k s a -> IndexedTraversal' i s a
reindexedBy f l = l . ifilteredBy f

-- }}}

-- {{{ foldl
foldingOf :: CF.Fold a r -> Fold s a -> s -> r
foldingOf (CF.Fold g x r) l s = r (foldlOf l g x s)

-- foldling :: CF.Fold a r -> Fold s a -> Getter s r
-- foldling fo l = to (CF.foldOver l fo)

-- mapOfWith :: Ord k => CF.Fold a v -> IndexedFold k s a -> s -> Map k v
-- mapOfWith f i s = s
--     & itoListOf i
--     & CF.fold (CF.foldByKeyMap f)


scanlOf :: (a -> b -> b) -> b -> Traversal s t a b -> s -> t
scanlOf f z l s = flip evalState z $ forOf l s \a -> do
    b <- get
    let !z' = f a b
    put z'
    pure z'

scanOver :: Traversal s t a b -> CS.Scan a b -> s -> t
scanOver l (CS.Scan f z) s = flip CMSS.evalState z $ forOf l s f
-- }}}

observe :: (State.MonadState s m) => LensLike' ((,) a) s a -> m a
observe l = State.state (l (\x -> (x, x)))

-- * Wrapped isomorphisms
-- {{{
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
-- }}}

lmapped :: Profunctor f => ASetter (f a r) (f b r) b a
lmapped f = Identity #. lmap (runIdentity #. f)

_Pure :: Prism' [a] a
_Pure = prism'
    do pure
    do \case
        [a] -> Just a
        _ -> Nothing

foldMapOf#
    :: (Coercible r w, Monoid w)
    => Iso' r w -> Getting w s a -> (a -> r) -> s -> r
foldMapOf# c l f = review c #. foldMapOf l (view c #. f)

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

indented :: Profunctor p => Int -> p a (Const Text a) -> p a (Const Text a)
indented n = setting rmap . #_Const %~ indent n

intercalatedBy :: Text -> Fold s a -> LensLike' (Const Text) s a
intercalatedBy n l f = Const . DT.intercalate n . fmap (getConst . f) . toListOf l 

intercalated :: Foldable f => Text -> LensLike' (Const Text) (f a) a
intercalated n = intercalatedBy n folded

indent :: Int -> Text -> Text
indent 0 = id
indent n = over (lines . mapped) (DT.replicate n " " <>)

lines :: Iso' Text [Text]
lines = iso (DT.splitOn "\n") (DT.intercalate "\n")

