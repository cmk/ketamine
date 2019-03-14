-- | Strict 'RWS.RWST' transformer that avoids the space-leak problem of the
-- traditional @Control.Monad.Trans.RWS.Strict@ and
-- @Control.Monad.Trans.RWS.Lazy@ implementations.
module Numeric.Ketamine.Effect.RWS
    (
    -- * MonadReader
      Reader.MonadReader (..)
    , Reader.asks

    -- * MonadWriter
    , Writer.MonadWriter (..)
    , Writer.listens
    , Writer.censor

    -- * MonadState
    , State.MonadState   (..)
    , State.modify
    , State.gets

    -- * RWS
    , RWS.RWS
    , RWS.rws
    , RWS.runRWS
    , RWS.evalRWS
    , RWS.execRWS
    , RWS.mapRWS
    , RWS.withRWS

    -- * RWST
    , RWS.RWST
    , RWS.rwsT
    , RWS.runRWST
    , RWS.evalRWST
    , RWS.execRWST
    , RWS.mapRWST
    , RWS.withRWST
    ) where

import qualified Control.Monad.Trans.RWS.CPS as RWS
import qualified Numeric.Ketamine.Effect.Reader          as Reader
import qualified Numeric.Ketamine.Effect.State           as State
import qualified Numeric.Ketamine.Effect.Writer          as Writer

-- This module just re-exports the relevant instances and concrete type(s),
-- avoiding any form of orphan instances.
