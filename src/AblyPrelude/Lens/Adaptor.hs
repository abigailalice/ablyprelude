
{-# LANGUAGE OverloadedLabels, QuantifiedConstraints #-}

module AblyPrelude.Lens.Adaptor
    ( view
    , reindexed
    , toListOf
    , toListAOf
    , interspersed
    , ht
    , hf
    , test
    , overview
    , adapt
    , open_
    ) where

import qualified Data.List.Final as DLF
import AblyPrelude hiding (view, reindexed, reverse, partitionThese, toListOf, uncons, unsnoc)
import AblyPrelude.Lens (_Identity, _Const)

import qualified Data.Text as DT
import qualified Control.Lens as CL
import qualified Control.Lens.Internal.Fold as CLIF

-- }}}

open_ :: Profunctor p => p a (Const (CLIF.Traversed () m) b) -> p a (m ())
open_ = adapted

adapt :: forall t b a p. (Coercible b t, Profunctor p) => p a b -> p a t
adapt = rmap coerce

adapted :: (Coercible (m b) (n t), Profunctor p)
    => p a (m b) -> p a (n t)
adapted = rmap coerce

ht :: Profunctor p
    => Gettable c s a
    -> p w s -> p w c
ht p = rmap (view p)

hf :: Profunctor p
    => AnIso c c s s
    -> p w s
    -> p w c
hf p = ht (from p)

type Gettable r s a = (a -> Const a a) -> (s -> Const r s)

hft :: Profunctor p
    => AnIso l l r r
    -> Gettable r s a
    -> p w s
    -> p w l
hft f t = hf f . ht t

overview :: ASetter s t a b -> Gettable b a x -> s -> t
overview f g s = over f (view g) s

test :: [(a, b)] -> [a]
test = (ht _Identity . mapped . hft _Identity _Const . _1 . hf _Const) id

-- h :: [(a, b)] -> [a]
-- h = (from _Identity . mapped . _Identity) _


view :: MonadReader s m => Gettable r s a -> m r
view l = do
    s <- ask
    pure $ getConst (l Const s)

toListOf :: (MonadReader s m, Foldable f)
    => ((a -> Const (DLF.List a) a) -> s -> Const (f a) s)
    -> m [a]
toListOf l = do
    s <- ask
    pure $ toList $ getConst (l (Const . pure) s)

toListAOf :: (MonadReader s m, Alternative f)
    => ((a -> Const a a) -> (s -> Const (DLF.List a) s))
    -> m (f a)
toListAOf l = do
    s <- ask
    pure $ getAlt $ foldMap (Alt . pure) $ getConst $ l Const s

-- folded
--     :: (Foldable f)
--     => (a -> Const r a') -> f a -> Const (MList r) a''
-- folded f s = foldMap (Const . pure . getConst . f) s

reindexed :: CL.Indexable i p => (k -> i) -> p s (m a) -> CL.Indexed k s (m a)
reindexed i g = Indexed \k s -> CL.indexed g (i k) s

interspersed :: Text -> (s -> Const (DLF.List Text) a) -> (s -> Const Text a)
interspersed t = out (DT.intercalate t . toList)

-- setView :: ((a -> Const a a) -> (s -> Identity t)) -> s -> t
-- setView f s = runIdentity (f Const s)
--
-- swp :: (a -> Const b b) -> a -> Identity b
-- swp f = Identity . getConst . f

-- interspersedOf :: Text -> LensLike' (Const (Endo [Text])) s a -> Getting Text s a
-- interspersedOf m f g s = _ $ CL.toListOf (f . to g) s

