
module AblyPrelude.Lens.Withered
    ( type WitherLike
    , type WitherLike'
    , type Wither
    , type Wither'
    , type IndexedWitherLike
    , type IndexedWitherLike'
    , type IndexedWither
    , type IndexedWither'

    , forOf
    , witherOf
    , filterOf
    , mapMaybeOf
    , catMaybesOf
    , iforOf
    , iwitherOf
    , ifilterOf
    , imapMaybeOf

    , withered
    , filtered
    , iwithered
    , ifiltered

    , guarded
    , guardedM
    , witherPrism
    , nonEmpty

    , ordNubOf
    , ordNubOfOn
    ) where

import Prelude hiding (filter)
import qualified Data.List.NonEmpty as DLN
import Data.Maybe
import Control.Applicative
import Control.Lens hiding (filtered, ifiltered, forOf, iforOf)
import Control.Monad
import qualified Data.Set as DS
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import qualified Witherable as W

type WitherLike f s t a b = (a -> MaybeT f b) -> s -> f t

type WitherLike' f s a = WitherLike f s s a a

type Wither s t a b = forall f. Applicative f => WitherLike f s t a b

type Wither' s a = Wither s s a a

type IndexedWitherLike k f s t a b = forall p. Indexable k p => p a (MaybeT f b) -> s -> f t

type IndexedWitherLike' k f s a = IndexedWitherLike k f s s a a

type IndexedWither k s t a b = forall f. Applicative f => IndexedWitherLike k f s t a b

type IndexedWither' k s a = IndexedWither k s s a a

-- {{{ Consumers of optics

-- |This is 'iforOf' with a more relaxed type. I worry this might cause
-- type-inference issues, but it is necessary for monadic changing optics
iforOf :: (Indexed k a fb -> s -> ft) -> s -> (k -> a -> fb) -> ft
iforOf l s f = l (Indexed f) s

-- |This is 'forOf' with a more relaxed type. I worry this might cause
-- type-inference issues, but it is necessary for monadic changing optics. This
-- can be made even more relaxed, using 'afb' in place of 'a -> fb'.
forOf :: ((a -> fb) -> s -> ft) -> s -> (a -> fb) -> ft
forOf = flip

witherOf
    :: ((a -> MaybeT m b) -> s -> m t)
    -> (a -> m (Maybe b))
    -> s
    -> m t
witherOf l f s = forOf l s (MaybeT . f)

iwitherOf
    :: (Indexed k a (MaybeT m b) -> s -> m t)
    -> (k -> a -> m (Maybe b))
    -> s
    -> m t
iwitherOf l f s = iforOf l s \k a -> MaybeT (f k a)

filterOf
    :: ((a -> MaybeT Identity a) -> s -> Identity s)
    -> (a -> Bool)
    -> s -> s
filterOf l f s = runIdentity $ forOf l s \a -> guard (f a) *> pure a

ifilterOf
    :: (Indexed k a (MaybeT Identity a) -> s -> Identity s)
    -> (k -> a -> Bool) -> s -> s
ifilterOf l f s = runIdentity $ iforOf l s \k a -> guard (f k a) *> pure a

mapMaybeOf
    :: WitherLike Identity s t a b
    -> (a -> Maybe b)
    -> s -> t
mapMaybeOf l f s = runIdentity $ forOf l s (MaybeT . Identity . f)

imapMaybeOf
    :: (Indexed k a (MaybeT Identity b) -> s -> Identity t)
    -> (k -> a -> Maybe b)
    -> s -> t
imapMaybeOf l f s = runIdentity $ iforOf l s \k a -> MaybeT $ Identity $ f k a

catMaybesOf
    :: WitherLike Identity s t (Maybe a) a
    -> s -> t
catMaybesOf l s = mapMaybeOf l id s

-- }}}

-- {{{ Withers

filtered :: W.Filterable f => WitherLike' Identity (f a) a
filtered f s = Identity $ W.filter (isJust . runIdentity . runMaybeT . f) s

ifiltered :: W.FilterableWithIndex k f => IndexedWitherLike' k Identity (f a) a
ifiltered f s = Identity $ W.ifilter (\k a -> isJust $ runIdentity $ runMaybeT $ indexed f k a) s

withered :: (W.Witherable t) => Wither (t a) (t b) a b
withered f s = W.wither (runMaybeT . f) s

iwithered :: (W.WitherableWithIndex k t) => IndexedWither k (t a) (t b) a b
iwithered f s = W.iwither (\k a -> runMaybeT $ indexed f k a) s

-- }}}

-- {{{ LensLike with Alternative

-- @'guarded'@ is similar to @'filtered'@, except that @'filtered'@ traverses
-- elements which pass the filter, but doesn't remove them. @'guarded'@ removes
-- them as well, though will typically need something to discharge the
-- @'Alternative'@ constraint.
guarded :: Alternative m => (a -> Bool) -> LensLike m a b a b
guarded p f a
    | p a = f a
    | otherwise = empty

-- |@'guarded'@ but with an effect. Again, see @'filtered'@.
guardedM :: (Alternative m, W.Filterable m) => (a -> m Bool) -> LensLike m a b a b
guardedM p f a = W.catMaybes (guard <$> p a) *> f a

-- |@'witherPrism'@ traverses under the prism, throwing an exception up if the
-- element isn't present. If used after a @'withered'@ this will remove the
-- element.
witherPrism :: (Alternative m, W.Filterable m) => Prism s t a b -> LensLike m s t a b
witherPrism l f s = withPrism l \proj inj -> case inj s of
    Left _ -> empty
    Right a -> fmap proj . f $ a

-- |@'nonEmpty'@ catches errors in its elements, filtering them out, unless all
-- elements are filtered out, in which case the error is propagated.
nonEmpty :: (Alternative m, W.Filterable m) => LensLike m (DLN.NonEmpty a) (DLN.NonEmpty b) a b
nonEmpty f = W.catMaybes . fmap DLN.nonEmpty . W.wither (optional . f) . DLN.toList

-- }}}

ordNubOf :: forall r s. Ord r
    => WitherLike' (State (DS.Set r)) s r
    -> s -> s
ordNubOf l s = evalState (witherOf l go s) DS.empty
  where
    go :: r -> State (DS.Set r) (Maybe r)
    go r = do
        visited <- get
        if DS.member r visited
        then pure Nothing
        else do
            put (DS.insert r visited)
            pure (Just r)

ordNubOfOn :: Ord r => WitherLike' (State (DS.Set r)) s a -> (a -> r) -> s -> s
ordNubOfOn l f s = evalState (witherOf l go s) DS.empty
  where
    go a = do
        visited <- get
        if DS.member (f a) visited
        then pure Nothing
        else do
            put (DS.insert (f a) visited)
            pure (Just a)

