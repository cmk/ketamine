{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | [Computation type:] Computations which read values from a shared environment.
--
-- [Binding strategy:] Monad values are functions from the environment to a value.
-- The bound function is applied to the bound value, and both have access
-- to the shared environment.
--
-- [Useful for:] Maintaining variable bindings, or other shared environment.
--
-- [Zero and plus:] None.
--
-- [Example type:] @'Reader' [(String,Value)] a@
--
-- The 'Reader' monad (also called the Environment monad).
-- Represents a computation, which can read values from
-- a shared environment, pass values from function to function,
-- and execute sub-computations in a modified environment.
module Numeric.Ketamine.Effect.Reader
    (
    -- * MonadReader
      MonadReader     (..)
    , asks

    -- * Reader
    , Reader.Reader
    , Reader.runReader
    , Reader.mapReader
    , Reader.withReader

    -- * ReaderT
    , Reader.ReaderT  (..)
    , Reader.mapReaderT
    , Reader.withReaderT

    -- * Lenses
    , view
    , review
    , preview

    -- ** Indexed
    , iview
    , ipreview

    -- * LiftLocal
    , Local.LiftLocal (..)
    , Local.defaultLiftLocal
    ) where

import Control.Lens.Getter       (Getting, IndexedGetting)
import Control.Lens.Indexed      (Indexed (Indexed))
import Control.Lens.Review       (AReview)
import Control.Monad.Trans.Class (MonadTrans (lift))

import Data.Functor.Const     (Const (Const, getConst))
import Data.Functor.Identity  (Identity (Identity, runIdentity))
import Data.Monoid            (First (First, getFirst))
import Data.Profunctor.Unsafe (( #. ), (.#))
import Data.Tagged            (Tagged (Tagged, unTagged))

import qualified Control.Lens                   as Lens
import qualified Control.Monad.Trans.Lift.Local as Local
import qualified Control.Monad.Trans.Reader     as Reader
import qualified Control.Monad.Trans.RWS.CPS    as RWS

-- FIXME: should revisit/consider the use of INLINE in a library and the
-- possible upstream occurences of SPECIALIZE.

class Monad m => MonadReader r m where
    {-# MINIMAL (ask | reader), local #-}

    -- | Retrieves the monad environment.
    ask :: m r
    ask = reader id
    {-# INLINE ask #-}

    -- | Executes a computation in a modified environment.
    local :: (r -> r)
          -- ^ The function to modify the environment.
          -> m a
          -- ^ @Reader@ to run in the modified environment.
          -> m a

    -- | Retrieves a function of the current environment.
    reader :: (r -> a)
           -- ^ The selector function to apply to the environment.
           -> m a
    reader f = fmap f ask
    {-# INLINE reader #-}

instance Monad m => MonadReader r (Reader.ReaderT r m) where
    ask = Reader.ask
    {-# INLINE ask #-}

    local = Reader.local
    {-# INLINE local #-}

    reader = Reader.reader
    {-# INLINE reader #-}

instance Monad m => MonadReader r (RWS.RWST r w s m) where
    ask = RWS.ask
    {-# INLINE ask #-}

    local = RWS.local
    {-# INLINE local #-}

    reader = RWS.reader
    {-# INLINE reader #-}

-- | Any transformer that can lift 'local' is a valid 'MonadReader' instance.
instance {-# OVERLAPPABLE #-}
    ( Local.LiftLocal t
    , Monad (t m)
    , MonadReader r m
    ) => MonadReader r (t m)
  where
    ask = lift ask
    {-# INLINE ask #-}

    local = Local.liftLocal ask local
    {-# INLINE local #-}

    reader = lift . reader
    {-# INLINE reader #-}

-- | Retrieves a function of the current environment.
asks :: MonadReader r m
     => (r -> a)
     -- ^ The selector function to apply to the environment.
     -> m a
asks = reader
{-# INLINE asks #-}

-- MonadReader constrained lenses

-- | View the value pointed to by a Getter, Iso or Lens or the result of
-- folding over all the results of a 'Lens.Fold' or 'Lens.Traversal' that
-- points at a monoidal value.
view :: MonadReader s m => Getting a s a -> m a
view l = ask >>= pure . Lens.view l
{-# INLINE view #-}

-- | View the index and value of an 'Lens.IndexedGetter' into the current
-- environment as a pair.
--
-- When applied to an 'Lens.IndexedFold' the result will most likely be a
-- nonsensical monoidal summary of the indices tupled with a monoidal summary
-- of the values and probably not whatever it is you wanted.
iview :: MonadReader s m => IndexedGetting i (i,a) s a -> m (i,a)
iview l = asks (getConst #. l (Indexed $ \i -> Const #. (,) i))
{-# INLINE iview #-}

-- | This can be used to turn an 'Iso' or 'Prism' around and view a value (or
-- the current environment) through it the other way.
review :: MonadReader b m => AReview t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE review #-}

-- | Retrieve the first value targeted by a 'Lens.Fold' or 'Lens.Traversal'
-- (or 'Just' the result from a 'Lens.Getter' or 'Lens.Lens').
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst #. Lens.foldMapOf l (First #. Just))
{-# INLINE preview #-}

-- | Retrieve the first index and value targeted by a 'Lens.Fold' or
-- 'Lens.Traversal' (or 'Just' the result from a 'Lens.Getter' or 'Lens.Lens').
ipreview :: MonadReader s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreview l = asks (getFirst #. Lens.ifoldMapOf l (\i a -> First (Just (i, a))))
{-# INLINE ipreview #-}
