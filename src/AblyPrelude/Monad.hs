
module AblyPrelude.Monad
    ( module X
    , _RWST
    , _RWS
    , local_
    ) where

import Prelude
import Control.Lens

import Control.Monad.State as X
import Control.Monad.Reader as X
import Control.Monad.Trans.Writer.CPS as X
import Control.Monad.Trans.Maybe as X (MaybeT(..))
import Control.Monad.Trans.RWS.CPS as X
    ( RWS, runRWS, rws
    , RWST, runRWST, rwsT
    )
import Control.Monad.Except as X
import Control.Monad.Cont as X
import Control.Monad.Random as X

local_ :: (MonadReader r m) => r -> m a -> m a
local_ r m = local (\_ -> r) m

_RWST :: (Monoid w, Functor m) => Iso' (RWST r w s m a) (r -> s -> m (a, s, w))
_RWST = iso runRWST rwsT

_RWS :: (Monoid w) => Iso' (RWS r w s a) (r -> s -> (a, s, w))
_RWS = iso runRWS rws
