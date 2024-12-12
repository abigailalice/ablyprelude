
{-# LANGUAGE PackageImports, PatternSynonyms, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeFamilies, ViewPatterns, FlexibleInstances #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, ConstraintKinds #-}

module AblyPrelude
    ( module X
    , shuffleOf
    , fmap_
    , filterMap
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
    , intercalateOf
    , here
    , there
    , (<&&>)
    , sequenceOf
    , bound
    , joined
    ) where

import qualified Prelude as Prelude
import qualified Data.Text.Lens as Lens
import qualified Control.Lens as Lens


import GHC.Generics as X (Generic)
import Data.Typeable as X (Typeable)

import Data.Ord as X (comparing)
import qualified Data.List as DL
import qualified Data.List.NonEmpty as DLN

import AblyPrelude.Data as X hiding (readFile, writeFile, mapMaybe)
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
import qualified Data.Text as DT
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
    , filter
    , lex
    , lookup
    )
import GHC.Int as X (Int32, Int64)
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
import Control.Lens hiding (sequenceOf)
import Control.Lens as X hiding (sequenceOf)

bound :: Monad m => Monad m => (s -> m a) -> LensLike m s t a t
bound g f = g >=> f

joined :: Monad m => LensLike m (m a) t a t
joined = bound id

sequenceOf :: Applicative m => LensLike m s t (m a) a -> s -> m t
sequenceOf l = l id

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

intercalateOf :: Text -> Fold s Text -> s -> Text
intercalateOf sep l s = DT.intercalate sep $ toListOf l s

