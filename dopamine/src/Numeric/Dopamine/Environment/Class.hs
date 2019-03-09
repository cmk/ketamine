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
             Rank2Types,
             ScopedTypeVariables,
             StandaloneDeriving,
             TypeApplications
#-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Numeric.Dopamine.Environment.Class where

import Control.Applicative (Alternative(..),liftA2)
import Control.Exception.Safe 
import Control.Monad
import Control.Monad.Cont.Class 
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Morph 
import Control.Monad.Primitive
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
--import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Control.Monad.Trans.Identity (IdentityT(..), mapIdentityT)
import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Resource (MonadResource(..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Default
import Data.Functor.Identity
import Data.Maybe (maybe)
import Data.Void

import Numeric.Dopamine.Exception (EpisodeCompleted(..))

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.State.Class as State 
import qualified Control.Monad.Trans.Class as Trans

import Pipes.Core (Proxy(..), Server, Client, Effect)
import qualified Pipes.Core as P

import qualified Pipes.Prelude as P

{-
 -
 -
(+>>) 
  :: Monad m	 
  => (a -> Server a o m r)	 
  -> Client a o m r	 
  -> Effect m r

(\i -> if i >= 10 then respond Nothing else respond $ Just (i + 1)) +>> c

(\i -> if i >= 10 then respond 0 else respond (i + 1)) +>> c

type Client a' a = Proxy a' a () X
type Server b' b = Proxy X () b' b

todo: what is Server'?

P.stdinLn :: MonadIO m => Proxy x' x () String m ()

fromSocketN :: MonadIO m => Socket -> Int -> Server' Int B.ByteString m ()
fromSocketN sock = loop where
    loop = \nbytes -> do
        bs <- liftIO (NSB.recv sock nbytes)
        if B.null bs
           then return ()
           else respond bs >>= loop
-}


--(>\\) = P.(>\\)

s :: MonadIO m => Int -> P.Server' Int Int m ()
s = loop where
    loop = \i -> do
        liftIO $ print i --bs <- liftIO (NSB.recv sock nbytes)
        if i > 9
           then return ()
           else P.respond (i + 1) >>= loop

s' :: MonadState Int m => Int -> P.Server' Int Int m Int
s' = loop where
    loop = \i -> do
        if i > 9
           then return i
           else State.modify (+ i) >> P.respond (i + 1) >>= loop

--c :: MonadIO m => P.Client' Int Int m ()
c :: MonadState Int m => P.Client' Int Int m Int
c = (State.get >>= P.request) >>= loop
  where
    loop i = (State.get >>= \j -> State.put (i+j) >> P.request (i + j + 1) ) >>= loop

go :: State Int Int
go = P.runEffect $ s' P.+>> c
 
--c = P.request 1
--foo = (\i -> if i >= 10 then P.respond 0 else P.respond (i + 1)) P.+>> P.request 1

f :: P.Proxy () Integer () Integer Maybe r
f = P.mapM $ \i -> if i >= 10 then Nothing else Just (i + 1)


-- 
newtype AgnT i o m r = AgnT { unAgnT :: P.Client i o m r }
  deriving
    ( Functor
    , Applicative
    , MFunctor
    , MMonad
    , Monad
    , MonadCatch
    , MonadIO
    , MonadThrow
    , MonadTrans
    )

newtype EnvT i o m r = EnvT { unEnvT :: P.Server i o m r }
  deriving
    ( Functor
    , Applicative
    , MFunctor
    , MMonad
    , Monad
    , MonadCatch
    , MonadIO
    , MonadThrow
    , MonadTrans
    )

deriving instance MonadState s m => MonadState s (EnvT i o m)
deriving instance MonadReader r m => MonadReader r (EnvT i o m)


instance PrimMonad m => PrimMonad (EnvT i o m) where

  type PrimState (EnvT i o m) = PrimState m
  
  primitive = Trans.lift . primitive

runEnvT :: Monad m => EnvT () Void m r -> m r
runEnvT = P.runEffect . unEnvT

class (MMonad (e i o), Monad (m s)) => MonadEnv e i o m s where

  interactEnv :: e () Void (m s) r -> m s r

  respond :: o -> e i o (m s) i

  viewEnv :: (s -> m s (Maybe o)) -> e i o (m s) r -- e i o m (Maybe o)

instance MonadState s (m s) => MonadEnv (P.Proxy Void ()) i o m s where

  interactEnv = P.runEffect --runEnvT -- evalState (runEnvT e)

  respond = P.respond

class (MMonad (a i o), Monad (m s)) => MonadAgent a i o m s where

  request :: i -> a i o (m s) o


instance MonadState s (m s) => MonadAgent AgnT i o m s where



{-
class (MonadAgent a i o m, MonadEnv e i o m) =>
  MonadEpisode a e i o m where

  interact :: (i -> e i o m r) -> a i o m r -> m r

interact 
  :: MonadEnv   e i o m s
  => MonadAgent a i o m s'
  => 
  => (i -> e i o m r) -> a i o m r -> m r
-}

{-
request :: Monad m => a' -> Proxy a' a y' y m a

runEnvT :: EnvT o m a -> (a -> m o) -> m o
runEnvT = runContT . unEnvT

respond :: i -> e i o m o

runEffect :: Monad m => Effect m r -> m r

(+>>)
  :: Monad m =>
     (b' -> Proxy a' a b' b m r)
     -> Proxy b' b c' c m r -> Proxy a' a c' c m r

class (Monad m, MonadTrans e) => MonadEnv s o m e | m -> s, e -> o where

  lowerEnv :: e m (Maybe o) -> m (Maybe o)

  viewEnv :: (s -> m (Maybe o)) -> e m (Maybe o)

  withEnv :: ((b -> m (Maybe o)) -> a -> m (Maybe o)) -> e m a -> e m b


instance MonadState s m => MonadEnv s o m (EnvT (Maybe o)) where

  lowerEnv e = runEnvT e return

  viewEnv v = Trans.lift $ State.get >>= v

  withEnv f e = EnvT . ContT $ runEnvT e . f
-}

{-
episode :: Monad m => o -> (a -> m (Maybe o)) -> (o -> m a) -> m ()


-- from dev/finalize-base-api
type Env s o = EnvT s Identity o

newtype EnvT s m o = EnvT { runEnvT :: StateT s (MaybeT m) o } deriving Functor


class Monad m => MonadEnv s m | m -> s where

    liftEnv :: Env s o -> m o

    -- stepEnv :: (o -> m a) -> m o -> m o
    -- runSelectT :: SelectT a m o -> (o -> m a) -> m o

    --stepEnv :: a -> m (Maybe (Outcome o r))
    
    overEnv :: (s -> s) -> m o -> m o

    -- run env from a default initial state and produce outcome state
    initEnv :: Default s => m o -> m (o, s)

instance Monad m => MonadEnv s (EnvT s m) where

    liftEnv = hoist generalize

    --stepEnv amo = EnvT $ \_ -> runEnvT mo a

    overEnv f = mapEnvT ((fmap . fmap) f)

    initEnv env = EnvT $ do
      mx <- Trans.lift . Trans.lift . runMaybeT . (`runStateT` def) $ runEnvT env
      case mx of
        Nothing -> mzero
        Just out -> pure out

-}












