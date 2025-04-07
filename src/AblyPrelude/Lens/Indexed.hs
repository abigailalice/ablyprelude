
module AblyPrelude.Lens.Indexed where

import Prelude
import AblyPrelude.Lens
import qualified Control.Monad as CM
-- import Control.Applicative
-- import Control.Lens
import qualified Control.Lens.Internal.Fold as CLIF

-- gets the target of the traversal, and make it available as the index
get :: Indexable a p => p a f_b -> a -> f_b
get = selfIndex

-- set the target of the traversal
put :: (Contravariant f, Profunctor p) => a -> p a (f a) -> p s (f s)
put a = to (\_ -> a)

-- Run an effect in the optic, passing its result back in the index. The
-- restriction that `m` must be the same in the optic should be fine in the
-- event we're running the optic with `forOf` or `forEach`, but with `forOf_`
-- you likely want `lift_`, which allows the `m` in the optic to differ from
-- the `m` in the action we're passing.
lift :: Monad m => m k -> IndexedLensLike k m s a s a
lift m f s = do
    k <- m
    indexed f k s

lift' :: Monad m 
    => m k -> IndexedLensLike k (Effect m) s a s a
lift' m f s = effect do
    k <- m
    getEffect $ indexed f k s

-- {{{ Qualified Do
(>>=)
    :: (Indexed k a fb -> s_ft)
    -> (k -> m_fn -> a -> fb)
    -> m_fn -> s_ft
z >>= f = \innerF -> z $ Indexed $ \k -> f k innerF

(*>), (>>) :: (b -> c) -> (a -> b) -> (a -> c)
(*>) = (.)
(>>) = (.)

pure :: k -> IndexedLensLike k f a b a b
pure k = \f a -> indexed f k a

(<$>) :: Indexable k' p => (k -> k') -> (Indexed k a b -> r) -> p a b -> r
(<$>) = reindexed

(<*>) :: Indexable p c
    => (Indexed (j -> p) s t -> r)
    -> (Indexed j a b -> s -> t)
    -> c a b -> r
(<*>) = icompose ($)

-- An alternative implementation of this would use 'mempty' or 'empty' for the
-- failure case, though this version is more general, as it doesn't require
-- additional constraints, and can be used to stop updates to parts of the
-- structure. A different version would support polymorphic lenses however.
guard :: Applicative m => Bool -> LensLike' m s s
guard True f s = f s
guard False _ s = Prelude.pure s

fail :: Applicative m => String -> LensLike' m s s
fail _ _ s = Prelude.pure s
-- }}}

_Index :: LensLike Identity (Indexed k a b -> r) (Indexed k' a b -> r) k k'
_Index = setting reindexed


type Effect m = Const (CLIF.Traversed () m)
-- {{{ Effect
effect :: m () -> Effect m a
effect = Const #. CLIF.Traversed

getEffect :: Effect m a -> m ()
getEffect = CLIF.getTraversed #. getConst

phantom :: Effect m a -> Effect m b
phantom = Const #. CLIF.Traversed #. CLIF.getTraversed #. getConst

acting :: forall m s a. Functor m => LensLike' m s a -> LensLike' (Effect m) s a
acting f g = Const #. CLIF.Traversed . CM.void . f (\x -> x <$ g' x)
  where
    g' :: a -> m ()
    g' = CLIF.getTraversed #. getConst #. g

act :: Monad m => (s -> m a) -> LensLike' (Effect m) s a
act g f = effect #. (g CM.>=> getEffect #. f)

acts :: Monad m => LensLike' (Effect m) (m a) a
acts = act id


-- }}}

-- {{{ Polymorphic
-- a -> Lens s t a t
polyput :: a -> (a -> t) -> s -> t
polyput a f _ = f a

-- t -> Traversal s t s b
polyreturn :: Applicative f => t -> (s -> f b) -> s -> f t
polyreturn t f s = f s Prelude.*> Prelude.pure t

polyact :: Monad m => (s -> m a) -> LensLike m s t a t
polyact f = (CM.>=>) f

polyacts :: Monad m => LensLike m (m a) (m a) a (m a)
polyacts = polyact id
-- }}}



