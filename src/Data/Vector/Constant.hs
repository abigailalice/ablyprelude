
module Data.Vector.Constant
    ( Vector(..)
    ) where

data Vector = Vector
    { forwardReference :: Vector Int
    , backReference    :: Vector Int
    , size             :: IORef Int
    }

new :: Int -> m Vector
new n = Vector <$> Vector.unsafeNew n <*> Vector.unsafeNew n

set :: Vector -> Int -> m ()
set (Vector f b) n = do
    n' <- Vector.read f n
    if (n' >= Vector.length b)
        then do
            n'' <- push b n
            Vector.write f n n''
        else
            Vector.write b n' n
  where
    -- push the value and return its index
    push :: Vector a -> a -> m Int

get :: Vector -> Int -> m Bool
get (Vector f b) n = do
    n' <- Vector.read f n
    if (n' >= Vector.length b)
        then pure False
        else do
            n'' <- Vector.read b n'
            pure (n == n'')

