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
module Numeric.Ketamine.Effect.Ref.Unboxed
    (

    -- * Lenses
      view
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
import Data.Tagged            (Tagged (Tagged, unTagged))

import qualified Control.Lens                   as Lens
import qualified Control.Monad.Trans.Reader     as Reader
import qualified Control.Monad.Trans.RWS.CPS    as RWS


import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Primitive      (PrimMonad, PrimState, RealWorld)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class    (MonadState(..))
import           Control.Monad.Primitive  (PrimMonad (..))
import           Data.Vector.Unboxed.Mutable (Unbox, MVector)
import qualified Data.Vector.Unboxed.Mutable as MUVector
import UnliftIO

import Numeric.Ketamine.Effect.Ref

-- | An unboxed rference. This works like an 'IORef', but the data is
-- stored in a bytearray instead of a heap object, avoiding
-- significant allocation overhead in some cases. For a concrete
-- example, see this Stack Overflow question:
-- <https://stackoverflow.com/questions/27261813/why-is-my-little-strf-int-require-allocating-gigabytes>.
--
-- The first parameter is the state token type, the same as would be
-- used for the 'ST' monad. If you're using an 'IO'-based monad, you
-- can use the convenience 'IORef' type synonym instead.
--

-- | Helpful type synonym for using a 'MVector' from an 'IO'-based stack.
--
--type IOMVector = MVector (PrimState IO)
--type STMVector = MVector (PrimState ST)

-- | Create a new 'MVector'
--
newVec :: (PrimMonad m, Unbox a) => a -> m (MVector (PrimState m) a)
newVec a = MUVector.replicate 1 a

-- | Read the value in a 'MVector'
--
readVec :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> m a
readVec v = MUVector.unsafeRead v 0

-- | Write a value into a 'MVector'. Note that this action is strict, and
-- will force evalution of the value.
--
writeVec :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> a -> m ()
writeVec v = MUVector.unsafeWrite v 0

-- | Modify a value in a 'MVector'. Note that this action is strict, and
-- will force evaluation of the result value.
--
modifyVec :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> (a -> a) -> m ()
modifyVec u f = readMVector u >>= writeMVector u . f

{-
readVec :: (MonadReader s m, HasRef b s, MonadIO m) => m b
readVec = view ref >>= liftIO . readRef

writeVec :: (MonadReader s m, HasRef a s, MonadIO m) => a -> m ()
writeVec st = view ref >>= liftIO . (`writeRef` st)

--uRefToRef :: Unbox a => MVector RealWorld a -> Ref a
uRefToRef rf = Ref (readRef rf) (writeRef rf)

-- | create a new unboxed Ref
--
newUnboxedRef :: (MonadIO m, Unbox a) => a -> m (Ref a)
newUnboxedRef a =
    uRefToRef <$> (liftIO $ newRef a)
-}

-- MonadReader constrained lenses

{-
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
-}
