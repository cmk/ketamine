{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | State effect and handlers.
module Numeric.Ketamine.Effect.State
    (
    -- * MonadState
      MonadState   (..)
    , modify
    , gets

    -- * State
    , State.State
    , State.runState
    , State.evalState
    , State.execState
    , State.mapState
    , State.withState

    -- * StateT
    , State.StateT (StateT)
    , State.runStateT
    , State.evalStateT
    , State.execStateT
    , State.mapStateT
    , State.withStateT

    -- * Lenses
    , use
    , over
    , assign
    , modifying
    ) where

import Control.Lens.Getter       (Getting)
import Control.Lens.Setter       (ASetter)
import Control.Monad.Trans.Class (MonadTrans (lift))

import qualified Control.Lens                     as Lens
import qualified Control.Monad.Trans.RWS.CPS      as RWS
import qualified Control.Monad.Trans.State.Strict as State

class Monad m => MonadState s m where
    {-# MINIMAL state | get, put #-}

    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))
    {-# INLINE get #-}

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))
    {-# INLINE put #-}

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
        s <- get
        let (a, s') = f s
        put s'
        pure a
    {-# INLINE state #-}

instance Monad m => MonadState s (State.StateT s m) where
    get = State.get
    {-# INLINE get #-}

    put = State.put
    {-# INLINE put #-}

    state = State.state
    {-# INLINE state #-}

instance Monad m => MonadState s (RWS.RWST r w s m) where
    get = RWS.get
    {-# INLINE get #-}

    put = RWS.put
    {-# INLINE put #-}

    state = RWS.state
    {-# INLINE state #-}

-- | Any transformer where the an underlying monad @m@ is 'MonadState' can be lifted.
instance {-# OVERLAPPABLE #-}
    ( MonadTrans t
    , Monad (t m)
    , MonadState s m
    ) => MonadState s (t m)
  where
    get = lift get
    {-# INLINE get #-}

    put = lift . put
    {-# INLINE put #-}

    state = lift . state
    {-# INLINE state #-}

-- | Monadic state transformer.
--
-- Maps an old state to a new state inside a state monad.
-- The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadState Int a) => a ()
--
-- This says that @modify (+1)@ acts over any
-- Monad that is a member of the @MonadState@ class,
-- with an @Int@ state.
--
-- Unlike mtl' @modify@, this is explicitly strict in the new state.
modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> let s' = f s in s' `seq` ((), s'))
{-# INLINE modify #-}

-- | Gets specific component of the state, using the supplied projection function.
gets :: MonadState s m => (s -> a) -> m a
gets f = fmap f get
{-# INLINE gets #-}

-- MonadState constrained lenses

use :: MonadState s m => Getting a s a -> m a
use l = gets (Lens.view l)
{-# INLINE use #-}

over :: MonadState s m => ASetter s s a a -> (a -> a) -> m ()
over l f = gets (Lens.over l f) >>= put
{-# INLINE over #-}

assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign l b = modify (Lens.set l b)
{-# INLINE assign #-}

modifying :: MonadState s m => ASetter s s a b -> (a -> b) -> m ()
modifying l f = modify (Lens.over l f)
{-# INLINE modifying #-}

