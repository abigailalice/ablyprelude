
{-# LANGUAGE PackageImports, PatternSynonyms, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeFamilies, ViewPatterns, FlexibleInstances #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module AblyPrelude
    ( module X
    , toMultiMapOf
    , shuffleOf
    , fmap_
    , filterMap
    , keys
    , keysWith
    , disjointUnion
    , bind
    , nf
    , nfIO
    , whnfIO
    , errorIO
    , show
    , pattern (:=>), type (:=>)
    , shuffle
    , shuffle1
    , type HasField'
    --, type List1
    , here
    , there
    , (<&&>)
    , sequenceOf
    , bound
    , rebound
    , joined
    , view
    , out
    , outward
    , into
    , intercalated
    , intercalatedBy
    , input
    , output
    , lines
    , indented
    , indented1
    , reindex
    ) where

import qualified Prelude as Prelude
import qualified Data.Text as DT
import qualified Data.Text.Lens as Lens
import qualified Data.Map as DM
import qualified Control.Lens as Lens


import GHC.Generics as X (Generic)
import Data.Typeable as X (Typeable)

import Data.Ord as X (comparing)
import qualified Data.List as DL
import qualified Data.List.NonEmpty as DLN

import AblyPrelude.Data as X hiding (readFile, writeFile)
import AblyPrelude.Monad as X
import Witherable as X
import Data.These as X

-- import Control.Monad.IO.Class as X (MonadIO(..))
import Data.Functor.Contravariant as X ((>$<))
import Data.Bifunctor as X (Bifunctor(..))
import Data.Semigroup.Foldable as X (Foldable1(..))
import GHC.Exts as X (IsList)
import Data.Functor as X (($>))
import Data.Foldable as X
import Data.Maybe as X (fromMaybe, isJust, isNothing)
import Data.Monoid as X
import Data.String as X (IsString(..))
-- import Data.Text as X (Text)
import Data.Text.IO as X (putStr, putStrLn)
import Data.Void as X (Void, absurd)
import Control.Applicative as X
-- import Control.Monad as X
import Prelude as X hiding
    ( undefined
    , putStr
    , putStrLn
    , show
    , String
    , read
    , head
    , span
    , lines
    , filter
    , lex
    , lookup
    )
import GHC.Int as X (Int16, Int32, Int64)
import Data.Bifoldable as X
import Data.Bitraversable as X

import Data.Kind as X (Type)
import Control.DeepSeq as X (NFData(..), force, deepseq)
import Data.Coerce as X (Coercible, coerce)
import Data.Proxy as X (Proxy(..))
import System.IO.Unsafe as X (unsafePerformIO)

import Data.Generics.Product.Fields as X (HasField)
import qualified Data.Generics.Product.Fields as DGPF
import qualified GHC.Records as GR

import "safe-exceptions" Control.Exception.Safe as X
    ( MonadThrow
    , MonadCatch
    , MonadMask(..)
    , mask_
    , throw
    , catch
    , handle
    , try
    , bracket
    , bracket_
    , finally )
import qualified Control.Exception as Exception
import qualified GHC.Stack as GS
import qualified System.Random as SR

import AblyPrelude.Development as X
import Control.Lens hiding (sequenceOf, view)
import Control.Lens as X hiding (sequenceOf, view)

bound :: Monad m => (s -> m a) -> LensLike m s t a t
bound g f = g >=> f

rebound :: Monad m => (b -> m t) -> LensLike m s t s b
rebound = flip bound

joined :: Monad m => LensLike m (m a) t a t
joined = bound id

-- rejoined :: Monad m => LensLike m s t s (m t)
-- rejoined = rebound id

sequenceOf :: Applicative m => LensLike m s t (m a) a -> s -> m t
sequenceOf l = l id

toMultiMapOf :: Ord k => IndexedFold k s a -> s -> DM.Map k (DLN.NonEmpty a)
toMultiMapOf f s = DM.fromListWith (<>) $ itoListOf (f <. to pure) s

keys :: Ord k' => Traversal (Map k v) (Map k' v) k k'
keys f = fmap DM.fromList . (traverse . _1) f . itoList

keysWith :: (Ord k') => (v -> v -> v) -> Traversal (Map k v) (Map k' v) k k'
keysWith g f = fmap (DM.fromListWith g) . (traverse . _1) f . itoList

disjointUnion :: Ord k => Map k a -> Map k b -> Map k (These a b)
disjointUnion as bs = DM.unionWith go (fmap This as) (fmap That bs)
  where
    go (This a) (That b) = These a b
    go _ _ = deadCode ""

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (fmap . fmap)

here :: Traversal (These a b) (These a' b) a a'
here f (This x) = This <$> f x
here f (These x y) = flip These y <$> f x
here _ (That y) = pure (That y)

there :: Traversal (These a b) (These a b') b b'
there _ (This x) = pure (This x)
there f (These x y) = These x <$> f y
there f (That y) = That <$> f y

type HasField' l s a = (DGPF.HasField' l s a, GR.HasField l s a)

shuffle :: forall a m. (MonadIO m) => [a] -> m [a]
shuffle = liftIO . fmap (fmap snd . DL.sortOn fst) . traverse go
  where
    go :: a -> IO (Double, a)
    go m = do
        n <- SR.randomIO
        pure (n, m)

shuffleOf :: MonadIO m => Traversal' s a -> s -> m s
shuffleOf l = partsOf l shuffle

shuffle1 :: forall a m. MonadIO m => DLN.NonEmpty a -> m (DLN.NonEmpty a)
shuffle1 = liftIO . fmap (fmap snd . DLN.sortWith fst) . traverse go
  where
    go :: a -> IO (Double, a)
    go m = do
        n <- SR.randomIO
        pure (n, m)

filterMap :: forall a b. (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr go []
  where
    go :: a -> [b] -> [b]
    go x ys = case f x of
        Just y -> y : ys
        Nothing -> ys

type (:=>) a b = (a, b)
pattern (:=>) :: a -> b -> (a, b)
pattern a :=> b = (a, b)

{-# INLINE fmap_ #-}
fmap_ :: (Functor f) => a -> f unit -> f a
fmap_ = (<$)

{-# INLINE bind #-}
bind :: (Monad m) => (a -> m b) -> m a -> m b
bind = (=<<)

show :: (Show a, Lens.IsText b) => a -> b
show = Lens.view Lens.packed . Prelude.show

-- |@'onto'@ is similar to @'to'@, applying a function to a getter, but rather
-- than applying it the target of the lens it applies it to the eventually
-- gotten value.
out :: Profunctor p => (r -> r') -> p s (Const r a) -> p s (Const r' a)
out f = output %~ Const . f . getConst

-- Getting a s a -> Getting r s a
into :: (Contravariant f, Profunctor p)
    => ((x -> Const x x) -> (s -> Const a s))
    -> Optic' p f s a
into f = to (view f)

-- Getting t r r -> p s (Const r) a -> p s (Const t a)
outward :: (Profunctor p)
    => ((x -> Const x x) -> (r -> Const t r))
    -> p s (Const r a) -> p s (Const t a)
outward g = out (view g)

indented :: Profunctor p => Int -> p s (Const Text a) -> p s (Const Text a)
indented n = out (indent n)

indented1 :: Profunctor p => Int -> p s (Const Text a) -> p s (Const Text a)
indented1 n = out (DT.drop n . indent n)

output :: Profunctor p => Setter (p a b) (p a b') b b'
output = setting rmap

input :: Profunctor p => Setter (p a b) (p a' b) a' a
input = setting lmap

intercalatedBy :: Text -> Fold s a -> LensLike' (Const Text) s a
intercalatedBy n l f = Const . DT.intercalate n . fmap (getConst . f) . toListOf l 

intercalated :: Foldable f => Text -> LensLike' (Const Text) (f a) a
intercalated n = intercalatedBy n folded

indent :: Int -> Text -> Text
indent 0 = id
indent n = over (lines . mapped) (DT.replicate n " " <>)

lines :: Iso' Text [Text]
lines = iso (DT.splitOn "\n") (DT.intercalate "\n")

reindex :: Indexable k p => (k' -> k) -> p s (m a) -> Indexed k' s (m a)
reindex k g = Indexed (\k' s -> indexed g (k k') s)

view :: MonadReader s m => ((a -> Const a a) -> s -> Const r s) -> m r
view l = do
    s <- ask
    pure $ getConst (l Const s)

{-# INLINE nfIO #-}
nfIO :: (X.NFData a, MonadIO m) => a -> m a
nfIO = liftIO . Exception.evaluate . X.force

{-# INLINE nf #-}
nf :: (X.NFData a) => a -> a
nf = X.force

{-# INLINE whnfIO #-}
whnfIO :: MonadIO m => a -> m a
whnfIO = liftIO . Exception.evaluate

{-# INLINE errorIO #-}
errorIO :: (GS.HasCallStack, MonadIO m) => [Char] -> m a
errorIO = liftIO . whnfIO . error

