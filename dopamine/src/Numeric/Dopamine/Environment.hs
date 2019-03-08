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


module Numeric.Dopamine.Environment where

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



data Outcome o r = Outcome { observation :: o, reward :: r }

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


shift :: MonadEnv s o m e => ((s -> m o) -> e m o) -> e m s
shift = undefined

shift' :: (MonadEnv s a Maybe e, MonadThrow m) => ((s -> m a) -> e Maybe a) -> e Maybe s
shift' f = shift $ f . (maybe (throwM SessionOver) return .)

shift''
  :: (MonadEnv s o m1 e, MonadThrow m2) =>
     (((a1 -> Maybe a2) -> a1 -> m2 a2) -> (s -> m1 o) -> e m1 o)
     -> e m1 s
shift'' f = shift . f $ (maybe (throwM SessionOver) return .) -- $ \g -> (foo . g)

bar :: MonadEnv s o Maybe e => e Maybe s
bar = shift' $ viewEnv


withAction :: (a -> StateT s Maybe o) -> a -> s -> Maybe o
withAction f a s = fmap fst . (`runStateT` s) $ f a

-- | Lift a pure state transition into an environment.
lift :: MonadEnv s o m e => (a -> StateT s Maybe o) -> a -> e m o
lift f a = view $ withAction f a

lift' :: MonadEnv s s m e => (a -> StateT s Maybe o) -> a -> e m s
lift' f a = view' . (isJust .) $ withAction f a

lift'' :: MonadEnv (Maybe s) o m e => (a -> StateT Bool Maybe o) -> a -> e m o
lift'' f a = view . (. isJust) $ withAction f a

lower :: MonadEnv s o m e => e m o -> m o
lower = lowerEnv

over :: MonadEnv s o m e => (m o -> m o) -> e m a -> e m a
over f = withEnv (f .)

-- | Use a state transition to advance environment one step.
step :: MonadEnv s o m e => (a -> m o) -> e m a -> m o
step f = lower . withEnv (const f)

-- | Use a pure state transition to advance environment one step.
step' :: forall a s o m e. MonadEnv s o m e => (a -> StateT s Maybe o) -> e m a -> m o
step' f = step $ lower . lift @_ @_ @_ @e f

step'' :: (MonadEnv s o m e, MonadThrow m, Default s) => (a -> StateT s Maybe o) -> e m a -> m o
step'' f = stepThrow $ \a -> withAction f a def

stepThrow :: (MonadEnv s o m e, MonadThrow m) => (a -> Maybe o) -> e m a -> m o
stepThrow f = step $ maybe (throwM SessionOver) return . f

view :: MonadEnv s o m e => (s -> Maybe o) -> e m o
view = viewEnv

view' :: MonadEnv s s m e => (s -> Bool) -> e m s
view' f = view $ \s -> bool Nothing (Just s) (f s)

views :: forall s o m e. MonadEnv s o m e => (s -> Maybe o) -> m o
views = lower @_ @_ @_ @e . view

views' :: forall s m e. MonadEnv s s m e => (s -> Bool) -> m s
views' = lower @_ @_ @_ @e . view'

viewsThrow :: (MonadEnv s o m e, MonadThrow m) => ((s -> m o) -> e m o) -> (s -> Maybe o) -> m o
viewsThrow f g = lower $ f $ maybe (throwM SessionOver) return . g



reset :: Monad m => EnvT a m a -> EnvT a' m a
reset e = Trans.lift $ runEnvT e return

--resetT :: Monad m => ContT r m r -> ContT r' m r

--shiftT :: Monad m => ((a -> m o) -> EnvT o m o) -> EnvT o m a

--fromEnv :: Functor m => EnvT s a m a -> EnvState s m a
--fromEnv = evalContT . runEnvT 

-- ContT o (MaybeT (StateT s m)) a 
-- (a -> StateT s Maybe o) -> StateT s Maybe o
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











