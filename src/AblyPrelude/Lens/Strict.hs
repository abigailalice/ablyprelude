
{-# LANGUAGE ExplicitNamespaces, PackageImports #-}
{-# LANGUAGE RankNTypes #-}

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
    ) where

import "base" Data.Coerce
import "base" Prelude (($!))
import "base" Control.Applicative
import "mtl" Control.Monad.State
import "lens" Control.Lens hiding (set')

{-# INLINE mapMonad' #-}
mapMonad' :: (Monad m) => (a -> b) -> m a -> m b
mapMonad' f xs = do x <- xs ; pure $! (f x)

{-# INLINE mapTraverse' #-}
mapTraverse' :: Traversable t => (a -> b) -> t a -> t b
mapTraverse' = over' traverse

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
        
