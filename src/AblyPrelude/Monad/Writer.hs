
module AblyPrelude.Monad.Writer
    ( -- WriterT(..)
    ) where

-- this is basically ListT, but where the `a` type is only stored at the end of
-- the list, and the remainder of the list consists of `w`. this might just be
-- the streaming libraries 
