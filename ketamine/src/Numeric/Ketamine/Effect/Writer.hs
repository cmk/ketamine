{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Strict 'Writer.WriterT' transformer using that avoids the space-leak
-- problem of the traditional @Control.Monad.Trans.Writer.Strict@ and
-- @Control.Monad.Trans.Writer.Lazy@ implementations.
module Numeric.Ketamine.Effect.Writer
    (
    -- * MonadWriter
      MonadWriter       (..)
    , listens
    , censor

    -- * Writer
    , Writer.Writer
    , Writer.runWriter
    , Writer.execWriter
    , Writer.mapWriter

    -- * WriterT
    , Writer.WriterT
    , Writer.runWriterT
    , Writer.execWriterT
    , Writer.mapWriterT

    -- * LiftPass
    , Pass.LiftPass     (liftPass)
    , Pass.defaultLiftPass

    -- * LiftListen
    , Listen.LiftListen (liftListen)
    , Listen.defaultLiftListen
    ) where

import Control.Monad.Trans.Class (MonadTrans (lift))

import qualified Control.Monad.Trans.Lift.Listen as Listen
import qualified Control.Monad.Trans.Lift.Pass   as Pass
import qualified Control.Monad.Trans.RWS.CPS     as RWS
import qualified Control.Monad.Trans.Writer.CPS  as Writer

class (Monad m, Monoid w) => MonadWriter w m where
    {-# MINIMAL (writer | tell), listen, pass #-}

    -- | @'writer' (a,w)@ embeds a simple writer action.
    writer :: (a, w) -> m a
    writer (a, w) = do
        tell w
        pure a
    {-# INLINE writer #-}

    -- | @'tell' w@ is an action that produces the output @w@.
    tell :: w -> m ()
    tell w = writer ((), w)
    {-# INLINE tell #-}

    -- | @'listen' m@ is an action that executes the action @m@ and adds
    -- its output to the value of the computation.
    listen :: m a -> m (a, w)

    -- | @'pass' m@ is an action that executes the action @m@, which
    -- pures a value and a function, and pures the value, applying
    -- the function to the output.
    pass :: m (a, w -> w) -> m a

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f m = do
    (a, w) <- listen m
    pure (a, f w)
{-# INLINE listens #-}

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the pure value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\x -> (x,f)) m)@
censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    pure (a, f)
{-# INLINE censor #-}

instance (Monad m, Monoid w) => MonadWriter w (Writer.WriterT w m) where
    writer = Writer.writer
    {-# INLINE writer #-}

    tell = Writer.tell
    {-# INLINE tell #-}

    listen = Writer.listen
    {-# INLINE listen #-}

    pass = Writer.pass
    {-# INLINE pass #-}

instance (Monad m, Monoid w) => MonadWriter w (RWS.RWST r w s m) where
    writer = RWS.writer
    {-# INLINE writer #-}

    tell = RWS.tell
    {-# INLINE tell #-}

    listen = RWS.listen
    {-# INLINE listen #-}

    pass = RWS.pass
    {-# INLINE pass #-}

-- | Any transformer that can lift 'pass' and 'listen' is a valid 'MonadWriter' instance.
instance {-# OVERLAPPABLE #-}
    ( Listen.LiftListen t
    , Pass.LiftPass t
    , Monad (t m)
    , MonadWriter r m
    ) => MonadWriter r (t m)
  where
    writer = lift . writer
    {-# INLINE writer #-}

    tell = lift . tell
    {-# INLINE tell #-}

    listen = Listen.liftListen listen
    {-# INLINE listen #-}

    pass = Pass.liftPass pass
    {-# INLINE pass #-}
