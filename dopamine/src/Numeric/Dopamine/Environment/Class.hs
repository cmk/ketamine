{-# LANGUAGE AllowAmbiguousTypes,
             ConstraintKinds, 
             MultiParamTypeClasses, 
             FlexibleContexts, 
             FlexibleInstances, 
             FunctionalDependencies, 
             GeneralizedNewtypeDeriving,
             UndecidableInstances, 
             TypeFamilies, 
             DeriveFunctor, 
             DeriveGeneric,
             ScopedTypeVariables, 
             TypeApplications
#-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Numeric.Dopamine.Environment.Class where

import Control.Applicative (Alternative(..),liftA2)
import Control.Exception.Safe 
import Control.Monad
import Control.Monad.Cont.Class 
import Control.Monad.IO.Class
import Control.Monad.Morph (MFunctor(..), MMonad(..), generalize)
import Control.Monad.Primitive
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.ST
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Control.Monad.Trans.Identity (IdentityT(..), mapIdentityT)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bool (bool)
import Data.Default
import Data.Functor.Identity
import Data.List (maximumBy, sort)
import Data.Maybe
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Vector ((!), Vector)
import Data.Word

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.State.Class as State 
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Vector.Generic as V
import qualified System.Random.MWC as R



data SessionOver = SessionOver
    deriving (Show, Typeable)

instance Exception SessionOver

{-
newtype StateST s a = StateST 
    { runStateST :: forall r. ReaderT (STRef r s) (ST r) a }

runState :: StateST s a -> s -> (a,s)
runState m s0 = runST (do
    r <- newSTRef s0
    a <- runReaderT (runStateST m) r
    s <- readSTRef r
    return (a,s))

instance Monad (StateST s) where
    return a = StateST (return a)
    m >>= f  = StateST (runStateST m >>= runStateST . f)

instance MonadState s (StateST s) where
    get   = State (ask >>= lift . readSTRef)
    put x = State (ask >>= \s -> lift (writeSTRef s x))

newtype StateST s m a = StateST { unStateST :: ReaderT s m a } 
  deriving (Functor, Applicative, Monad)



instance MonadTrans (StateST s) where

  lift = StateST . Trans.lift
      

-}



newtype EnvT o m a = EnvT { unEnvT :: ContT o m a }
  deriving (Functor, Applicative, Monad)

runEnvT :: EnvT o m a -> (a -> m o) -> m o
runEnvT = runContT . unEnvT

instance MonadState s m => MonadState s (EnvT o m) where

  state = EnvT . State.state


instance MonadThrow m => MonadThrow (EnvT o m) where

  throwM = EnvT . Catch.throwM


instance MonadTrans (EnvT o) where

  lift = EnvT . Trans.lift


instance PrimMonad m => PrimMonad (EnvT o m) where

  type PrimState (EnvT o m) = PrimState m
  
  primitive = Trans.lift . primitive


class (Monad m, MonadTrans e) => MonadEnv s o m e | m -> s, e -> o where

  lowerEnv :: e m o -> m o

  viewEnv :: (s -> Maybe o) -> e m o

  withEnv :: ((b -> m o) -> a -> m o) -> e m a -> e m b


instance (MonadThrow m, MonadState s m) => MonadEnv s o m (EnvT o) where

--  type State (EnvT (IORef s) s m) = EnvState (IORef s) m --(MaybeT m)

  --liftEnv f = State.state . runState . f
 
  lowerEnv e = runEnvT e return

  -- | view the current reward and/or observable state
  viewEnv v = Trans.lift $ do { s <- State.get; maybe (throwM SessionOver) return . v $ s }

  withEnv f m = EnvT . ContT $ runEnvT m . f

{-
  --overEnv f s = s >> do { S.modify' @R f; S.get @R }

  withEnv view env = s -- >> do { r <- S.get @R ; return . return . view $ r }
    where s = runContT . runEnvT $ env
-}

{-
instance HasState S r (EnvState (STRef s r) m) => MonadEnv S r o (EnvT (STRef s r) o m) where

  type State (EnvT (STRef s r) o m) = EnvState (STRef s r) (MaybeT m)

  initEnv = S.state @S

  loadEnv view = do { r <- S.get @S ; view r }

  overEnv f s = s >> do { S.modify' @S f; S.get @S }

Control.Monad.Trans.Cont.callCC ::
  forall k a (r :: k) (m :: k -> *) b.
  ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
cont :: ((a -> r) -> r) -> Cont r a
evalCont :: Cont r r -> r
evalContT :: Monad m => ContT r m r -> m r

liftLocal ::
  Monad m =>
  m r'
  -> ((r' -> r') -> m r -> m r)
  -> (r' -> r')
  -> ContT r m a
  -> ContT r m a

mapContT ::
  forall k (m :: k -> *) (r :: k) a.
  (m r -> m r) -> ContT r m a -> ContT r m a

resetT :: Monad m => ContT r m r -> ContT r' m r

shiftT :: Monad m => ((a -> m o) -> EnvT o m o) -> EnvT o m a

withContT ::
  forall k b (m :: k -> *) (r :: k) a.
  ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b

withEnvT :: 
-}

-- The State monad provides a minimal DSL sufficient for describing simple environments (e.g. pole cart)
--fromState :: forall s o m. MonadEnv s o m => State.State s o -> (State m) o
--fromState s = initEnv @s @_ @m $ State.runState s

--load :: forall t r o m a. MonadEnv t r o m => (r -> (State m) (Maybe o)) -> (State m) (Maybe o)
--load = loadEnv @t @_ @_ @m

--over :: forall t r o m a. MonadEnv t r o m => (r -> r) -> (State m) r -> (State m) r 
--over = overEnv @t @_ @_ @m

--step :: forall t r o m a. MonadEnv t r o m => m a -> (a -> State m o) -> State m o
--step = withEnv @t












