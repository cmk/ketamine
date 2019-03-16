{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | State effect and handlers.
module Numeric.Ketamine.Effect.State where


import Control.Lens.Getter       (Getting)
import Control.Lens.Setter       (ASetter)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Lens (Lens',lens)
import qualified Control.Lens as Lens

import Control.Monad.Primitive      (PrimMonad, PrimState, RealWorld)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class    (MonadState(..))
import           Control.Monad.Primitive  (PrimMonad (..))
import           Data.Vector.Unboxed.Mutable (Unbox)
import qualified Data.Vector.Unboxed.Mutable as MUVector
import UnliftIO
-- | Environment values with stateful capabilities to SomeRef
--
class HasStateRef s env | env -> s where
    stateRefL :: Lens' env (SomeRef s)

-- | Identity state reference where the SomeRef is the env
--
instance HasStateRef a (SomeRef a) where
    stateRefL = lens id (\_ x -> x)

-- | Abstraction over how to read from and write to a mutable reference
--
data SomeRef a = SomeRef !(IO a) !(a -> IO ())

-- | Read from a SomeRef
--
readSomeRef :: MonadIO m => SomeRef a -> m a
readSomeRef (SomeRef x _) = liftIO x

-- | Write to a SomeRef
--
writeSomeRef :: MonadIO m => SomeRef a -> a -> m ()
writeSomeRef (SomeRef _ x) = liftIO . x

-- | Modify a SomeRef
-- This function is subject to change due to the lack of atomic operations
--
modifySomeRef :: MonadIO m => SomeRef a -> (a -> a) -> m ()
modifySomeRef (SomeRef read' write) f =
    liftIO $ (f <$> read') >>= write

ioRefToSomeRef :: IORef a -> SomeRef a
ioRefToSomeRef ref = do
    SomeRef (readIORef ref)
            (\val -> modifyIORef' ref (\_ -> val))

uRefToSomeRef :: Unbox a => Ref RealWorld a -> SomeRef a
uRefToSomeRef ref = do
    SomeRef (readRef ref) (writeRef ref)

-- | create a new boxed SomeRef
--
newSomeRef :: MonadIO m => a -> m (SomeRef a)
newSomeRef a = do
    ioRefToSomeRef <$> newIORef a

-- | create a new unboxed SomeRef
--
newUnboxedSomeRef :: (MonadIO m, Unbox a) => a -> m (SomeRef a)
newUnboxedSomeRef a =
    uRefToSomeRef <$> (liftIO $ newRef a)


-- | An unboxed reference. This works like an 'IORef', but the data is
-- stored in a bytearray instead of a heap object, avoiding
-- significant allocation overhead in some cases. For a concrete
-- example, see this Stack Overflow question:
-- <https://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes>.
--
-- The first parameter is the state token type, the same as would be
-- used for the 'ST' monad. If you're using an 'IO'-based monad, you
-- can use the convenience 'IORef' type synonym instead.
--
newtype Ref s a = Ref (MUVector.MVector s a)

-- | Helpful type synonym for using a 'Ref' from an 'IO'-based stack.
--
--type IORef = Ref (PrimState IO)

-- | Create a new 'Ref'
--
newRef :: (PrimMonad m, Unbox a) => a -> m (Ref (PrimState m) a)
newRef a = fmap Ref (MUVector.replicate 1 a)

-- | Read the value in a 'Ref'
--
readRef :: (PrimMonad m, Unbox a) => Ref (PrimState m) a -> m a
readRef (Ref v) = MUVector.unsafeRead v 0

-- | Write a value into a 'Ref'. Note that this action is strict, and
-- will force evalution of the value.
--
writeRef :: (PrimMonad m, Unbox a) => Ref (PrimState m) a -> a -> m ()
writeRef (Ref v) = MUVector.unsafeWrite v 0

-- | Modify a value in a 'Ref'. Note that this action is strict, and
-- will force evaluation of the result value.
--
modifyRef :: (PrimMonad m, Unbox a) => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef u f = readRef u >>= writeRef u . f



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

