{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Ketamine.Types
  ( Keta (..)
  , runKeta
  , liftKeta
  -- * SomeRef for Writer/State interfaces
  , SomeRef
  , HasStateRef (..)
  , newSomeRef
  , newUnboxedSomeRef
  , readSomeRef
  , writeSomeRef
  , modifySomeRef
  ) where

import GHC.Exts (RealWorld)

import Control.Lens
import Control.Exception.Safe
import Control.Monad.IO.Class --(MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Primitive  (PrimMonad (..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans.Reader (ReaderT(..))
--import Data.IORef 
import UnliftIO
import Numeric.Ketamine.Effect.State
import Numeric.Ketamine.Effect.Random

import qualified System.Random.TF             as TF
import qualified System.Random.TF.Instances   as TF

-- | The Reader+IO monad. This is different from a 'ReaderT' because:
--
-- * It's not a transformer, it hardcodes IO for simpler usage and
-- error messages.
--
-- * Instances of typeclasses like 'MonadLogger' are implemented using
-- classes defined on the environment, instead of using an
-- underlying monad.
newtype Keta env a = Keta { unKeta :: ReaderT env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadThrow)

runKeta :: MonadIO m => env -> Keta env a -> m a
runKeta env (Keta (ReaderT f)) = liftIO (f env)

liftKeta :: (MonadIO m, MonadReader env m) => Keta env a -> m a
liftKeta rio = do
    env <- ask
    runKeta env rio

instance MonadUnliftIO (Keta env) where
    askUnliftIO = Keta $ ReaderT $ \r ->
                    withUnliftIO $ \u ->
                      return (UnliftIO (unliftIO u . flip runReaderT r . unKeta))

instance PrimMonad (Keta env) where
    type PrimState (Keta env) = PrimState IO
    primitive = Keta . ReaderT . const . primitive

instance HasStateRef s env => MonadState s (Keta env) where
    get = do
      ref <- view stateRefL
      liftIO $ readSomeRef ref

    put st = do
      ref <- view stateRefL
      liftIO $ writeSomeRef ref st

{-
tfRandom :: (HasStateRef TF.TFGen env, TF.Random a) => Keta env a
tfRandom = Keta $ do
    g <- get
    let (x, g') = TF.random g
    put $! g'
    return x
{-# INLINE tfRandom #-}
-}

{-
-- Generate a random-fu MonadRandom instance for tf-random.
-- Note that `getRandomDouble` will be generated automatically from
-- `getRandomWord64` by the TemplateHaskell.  (tf-random doesn't provide
-- a concrete instance itself.)
Source.monadRandom [d|
    instance HasStateRef TF.TFGen env => Source.MonadRandom (Keta env m) where
        getRandomWord8 = tfRandom
        getRandomWord16 = tfRandom
        getRandomWord32 = tfRandom
        getRandomWord64 = tfRandom
    |]
-}

{-
-- | The random monad parameterized by the underlying generator, @g@.
type Random g = Keta g Identity

-- | Run the monadic computation, returning the computed value and the
-- generator state.
runRandom :: g -> Random g a -> (a, g)
runRandom g = runIdentity . runKeta g
{-# INLINE runRandom #-}

-- | Run the monadic computation, returning the computed value and discarding
-- the generator state.
evalRandom :: g -> Random g a -> a
evalRandom g = runIdentity . evalKeta g
{-# INLINE evalRandom #-}

-- | The random transformer monad parameterized by:
--
-- * @g@ - The generator.
--
-- * @m@ - The inner monad.
--
newtype Keta g m a = Keta { unKeta :: StateT g m a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadTrans
        , MonadFix
        , MonadFail
        , MonadPlus
        , MonadIO
        )


-- | Run the monadic computation, returning the computed value and the
-- generator state.
runKeta :: g -> Keta g a -> IO (a, g)
runKeta g m = State.runStateT (unKeta m) g
{-# INLINE runKeta #-}

-- | Run the monadic computation, returning the computed value and discarding
-- the generator state.
evalKeta :: g -> Keta g a -> IO a
evalKeta g m = State.evalStateT (unKeta m) g
{-# INLINE evalKeta #-}


type instance StT (Keta g) a = StT (State.StateT g) a

instance HasRandom (Keta Std.StdGen) where
    random = Keta . Random.sample
    {-# INLINE random #-}

instance HasRandom (Keta Pure.PureMT) where
    random = Keta . Random.sample
    {-# INLINE random #-}

instance HasStateRef Dev.DevRandom env => HasRandom (Keta env) where
    random v = Keta $
        State.get >>= liftIO . Random.runRVar v . random
    {-# INLINE random #-}

instance {-# OVERLAPPABLE #-}
         ( PrimMonad m
         , PrimState m ~ s
         , HasStateRef (MWC.Gen s) env 
         ) => HasRandom (Keta env m) where
    random v = Keta $
        let f = primToPrim :: ST s a -> m a
         in State.get @(MWC.Gen s)
            >>= lift . f . flip Random.sampleFrom v
    {-# INLINE random #-}

instance HasStateRef (MWC.Gen RealWorld) env => HasRandom (Keta env) where
    random v = Keta $
        --let f = primToPrim :: IO a -> m a
        State.get >>= lift . flip Random.sampleFrom v . random
    {-# INLINE random #-}

instance Monad m => HasRandom (Keta TF.TFGen m) where
    random = Random.sample


-- | Run a random effect, seeded from the given hash.  Allows us to run
--
-- The `Hash64` seed can be constructed from an input source using 'hash64'.
--runSeededRandom :: HasStateRef TF.TFGen env => Murmur.Hash64 -> Keta TF.TFGen a -> IO a
runSeededRandom h = runKeta $ TF.seedTFGen (0, 0, 0, Murmur.asWord64 h)

--sampleSeeded :: (Monad m, Distribution d t) => Murmur.Hash64 -> d t -> m t
sampleSeeded s = runSeededRandom s . random . Random.rvar

-}


