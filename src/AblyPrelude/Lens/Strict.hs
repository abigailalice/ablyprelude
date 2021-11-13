
{-# LANGUAGE ExplicitNamespaces, PackageImports #-}
{-# LANGUAGE RankNTypes, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module AblyPrelude.Lens.Strict
    ( mapMonad'
    , mapTraverse'
    , traverse'
    , strictly
    , forOf'
    , over'
    , modifying'
    , set'
    , assign'
    , type AStrictSetter'
    , type AStrictSetter
    , type AnIndexedStrictSetter'
    , type AnIndexedStrictSetter
    , united'
    , united''
    , seqOf
    , whnfOf
    ) where

import Data.Monoid
import Control.DeepSeq
import "base" Data.Coerce
import "base" Prelude
import "base" Control.Applicative
import "mtl" Control.Monad.State
import "lens" Control.Lens hiding (set')

-- |@mapMonad'@ is strict in the returned values, not original values. Thus we
-- have
--
-- @
--    mapMonad' (const ()) (pure undefined) = pure ()
-- @
{-# INLINE mapMonad' #-}
mapMonad' :: (Monad m) => (a -> b) -> m a -> m b
mapMonad' f xs = do x <- xs ; pure $! (f x)

-- |@mapTraverse'@ is strict in the returned values, not original values. Thus
-- we have
--
-- @
--     mapTraverse' (const ()) (pure undefined) = pure ()
-- @
{-# INLINE mapTraverse' #-}
mapTraverse' :: Traversable t => (a -> b) -> t a -> t b
mapTraverse' = over (strictly traverse)

{-# INLINE traverse' #-}
traverse' :: (Traversable t, Applicative f)
            => (a -> f b) -> t a -> f (t b)
traverse' = strictly traverse

type AStrictSetter s t a b = LensLike (Strictly Identity) s t a b
type AStrictSetter' s a = AStrictSetter s s a a
type AnIndexedStrictSetter i s t a b = IndexedLensLike i (Strictly Identity) s t a b
type AnIndexedStrictSetter' i s a = AnIndexedStrictSetter i s s a a

newtype Strictly f a = Strictly (f a)
instance (Functor f) => Functor (Strictly f) where
    fmap f (Strictly x) = Strictly (fmap (f $!) x)
instance (Applicative f) => Applicative (Strictly f) where
    pure x = Strictly (pure x)
    Strictly x <*> Strictly y = Strictly (liftA2 ($!) x y)

-- |@'strictly'@ makes an optic strict over its returned value
{-# INLINE strictly #-}
strictly :: LensLike (Strictly f) s t a b -> LensLike f s t a b
strictly = coerce

{-# INLINE forOf' #-}
forOf' :: LensLike (Strictly f) s t a b -> s -> (a -> f b) -> f t
forOf' l s f = strictly l f s

{-# INLINE over' #-}
over' :: AStrictSetter s t a b -> (a -> b) -> s -> t
over' l = over (strictly l)

{-# INLINE modifying' #-}
modifying' :: (MonadState s m) => AStrictSetter s s a a -> (a -> a) -> m ()
modifying' l = modifying (strictly l)

{-# INLINE set' #-}
set' :: AStrictSetter s t a b -> b -> s -> t
set' l = set (strictly l)

{-# INLINE assign' #-}
assign' :: (MonadState s m) => AStrictSetter' s a -> a -> m ()
assign' l = assign (strictly l)

-- |@'seqOf'@ is a @seq@ which forces only those targets of the provided @Fold@.
-- This can be used to force fields of a data structure with different types, by
-- using @united'@ to convert a @Fold@ to the same type, and then appending the
-- folds. For instance, if we wanted to write a function that forces both
-- elements of a pair then we could write
--
-- @
--     forcePair :: (a, b) -> (a, b)
--     forcePair s = seqOf (_1 . united' <> _2 . united') s s
-- @
--
-- Without the calls to @united'@ this would have the type @(a,a) -> (a,a)@
-- instead, as appending @_1 <> _2@ would force the folds to have the same type.
-- Note that using @united@ provided from the @lens@ package will not force the
-- fields, as it doesn't force it's argument.
--
-- @
--     seq = seqOf id
-- @
{-# INLINE seqOf #-}
seqOf :: Getting (Endo ()) a unit -> a -> b -> b
seqOf l s b = foldrOf l seq () s `seq` b

-- |Evaluate every value targeted by the @Fold@ to weak head normal form. This
-- can be used to evaluate values of different types using @united'@, like
--
-- @
--     whnfOf (firstFold . united' <> secondFold . united')
-- @
--
-- Using @united''@ instead evaluates those elements to head normal form.
{-# INLINE whnfOf #-}
whnfOf :: Getting (Endo ()) a unit -> a -> a
whnfOf l s = seqOf l s s

-- |@united'@ is just like @Lens.united@, except that it's strict in its
-- argument. This is mostly useful together with @seqOf@.
{-# INLINE united' #-}
united' :: Lens' s ()
united' f x = x `seq` fmap (\_ -> x) (f ())

-- |@united''@ is just like @Lens.united@ except that it fully forces its
-- argument. This is mostly useful together with @seqOf@.
{-# INLINE united'' #-}
united'' :: NFData s => Lens' s ()
united'' f x = x `deepseq` fmap (\_ -> x) (f ())

