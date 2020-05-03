
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Map.Splay.Inf
    ( TMap(..)
    , Map(..)
    , Set(..)
    , at
    ) where

import AblyPrelude
import qualified Control.Lens as Lens

newtype Map k v = Map (TMap k (Maybe v))
instance (Ord k) => Lens.Ixed (Map k v) where
    ix k f (Map m) = Map <$> at k (traverse f) m
instance (Ord k) => Lens.At (Map k v) where
    at k f (Map m) = Map <$> at k f m
type instance Lens.Index (Map k v) = k
type instance Lens.IxValue (Map k v) = v

newtype Set k v = Set (TMap k Bool)
-- dense, with no extreme element
-- dense, but with 1 extreme element
-- dense, with 2 extreme elements
data TMap k v = Bin (TMap k v) k v (TMap k v)


at :: forall k v f. (Ord k, Functor f) => k -> Lens.LensLike' f (TMap k v) v
at k0 f = go
  where
    go :: TMap k v -> f (TMap k v)
    go (Bin l k v r) = case compare k0 k of
        -- in this case
        LT -> let Bin ll lk lv lr = l in case compare k0 lk of
            EQ -> f lv   <&> \lv -> zigL Bin ll lk lv lr k v r
            LT -> go ll <&> \(Bin lll llk llv llr) ->
                zigZigL Bin lll llk llv llr lk lv lr k v r
            GT -> go lr <&> \(Bin lrl lrk lrv lrr) ->
                zigZagL Bin ll lk lv lrl lrk lrv lrr k v r
        EQ -> f v <&> \v ->  Bin l k v r
        GT -> let Bin rl rk rv rr = r in case compare k0 rk of
            EQ -> f rv  <&> \rv -> zigR Bin l k v rl rk rv rr
            LT -> go rl <&> \(Bin rll rlk rlv rlr) ->
                zigZagR Bin l k v rll rlk rlv rlr rk rv rr
            GT -> go rr <&> \(Bin rrl rrk rrv rrr) ->
                zigZigR Bin l k v rl rk rv rrl rrk rrv rrr

-- these take the elements to splay based on their in-order sequence
zigL, zigR :: (s -> k -> v -> s -> s) -> s -> k -> v -> s -> k -> v -> s -> s
zigL bin  ll lk lv      lr k v r
    = bin ll lk lv (bin lr k v r)
zigR bin       l k v rl  rk rv rr
    = bin (bin l k v rl) rk rv rr
zigZigL, zigZigR, zigZagL, zigZagR :: (s -> k -> v -> s -> s)
    -> s -> k -> v -> s -> k -> v -> s -> k -> v -> s -> s
zigZigL bin lll llk llv      llr lk lv      lr k v r
    = bin   lll llk llv (bin llr lk lv (bin lr k v r))
zigZigR bin         l k v rl  rk rv rrl  rrk rrv rrr
    = bin (bin (bin l k v rl) rk rv rrl) rrk rrv rrr
zigZagL bin    ll lk lv lrl  lrk lrv      lrr k v r
    = bin (bin ll lk lv lrl) lrk lrv (bin lrr k v r)
zigZagR bin    l k v rll  rlk rlv      rlr rk rv rr
    = bin (bin l k v rll) rlk rlv (bin rlr rk rv rr)

