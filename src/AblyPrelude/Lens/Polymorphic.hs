
{-# LANGUAGE OverloadedLabels #-}

module AblyPrelude.Lens.Polymorphic
    ( view
    , reindexed
    , toListOf
    , toListAOf
    , onto
    , interspersed
    ) where

import qualified Data.List.Final as DLF
import AblyPrelude hiding (onto, view, reindexed, reverse, partitionThese, toListOf, interspersed, uncons, unsnoc)
import AblyPrelude.Lens ()

import qualified Data.Text as DT
import qualified Control.Lens as CL

-- }}}

view :: MonadReader s m => ((a -> Const a a) -> s -> Const r s) -> m r
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

onto :: Profunctor p => (r -> r') -> p s (Const r a) -> p s (Const r' a)
onto = over (setting rmap . #_Const)

-- folded
--     :: (Foldable f)
--     => (a -> Const r a') -> f a -> Const (MList r) a''
-- folded f s = foldMap (Const . pure . getConst . f) s

reindexed :: CL.Indexable i p => (k -> i) -> p s (m a) -> CL.Indexed k s (m a)
reindexed i g = Indexed \k s -> CL.indexed g (i k) s

interspersed :: Text -> (s -> Const (DLF.List Text) a) -> (s -> Const Text a)
interspersed t = onto (DT.intercalate t . toList)

-- setView :: ((a -> Const a a) -> (s -> Identity t)) -> s -> t
-- setView f s = runIdentity (f Const s)
--
-- swp :: (a -> Const b b) -> a -> Identity b
-- swp f = Identity . getConst . f

-- interspersedOf :: Text -> LensLike' (Const (Endo [Text])) s a -> Getting Text s a
-- interspersedOf m f g s = _ $ CL.toListOf (f . to g) s

