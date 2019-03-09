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
import Control.Monad.Morph -- (MFunctor(..), MMonad(..), MonadTrans(..)) 
import Control.Monad.Primitive
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Control.Monad.Trans.Identity (IdentityT(..), mapIdentityT)
import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Resource (MonadResource(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State
import Data.Default
import Data.Functor.Identity
import Data.Maybe (maybe)
import Data.Void

import Numeric.Dopamine.Exception (EpisodeCompleted(..))

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Reader.Class as Reader
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

newtype EnvT i o m r = EnvT { unEnvT :: ReaderT i (P.Server i o m) r }
  deriving
    ( Functor
    , Applicative
    --, MFunctor
    --, MMonad
    , Monad
    , MonadCatch
    , MonadIO
    , MonadThrow
    --, MonadTrans
    )

deriving instance MonadState s m => MonadState s (EnvT i o m)

--deriving instance MonadReader r m => MonadReader r (EnvT i o m)
instance Monad m => MonadReader i (EnvT i o m) where
  ask = EnvT Reader.ask
  local f m = EnvT $ Reader.local f (unEnvT m)

{-
instance MonadReader r m => MonadReader r (EnvT i o m) where
  ask = Trans.lift ask
  local f m = mapEnvT (Reader.local f) m
-}

mapEnvT :: (P.Server i o m a -> P.Server i o n b) -> EnvT i o m a -> EnvT i o n b
mapEnvT f e = undefined
  EnvT $ ReaderT $ \inp ->
    f (runReaderT (unEnvT e) inp)

instance MonadTrans (EnvT i o) where
  lift = liftEnvT . Trans.lift

instance MFunctor (EnvT i o) where
  hoist f =
    mapEnvT (hoist f)

-- TODO implement
instance MMonad (EnvT i o) where
  embed f = undefined

--deriving instance MonadState s m => MonadState s (EnvT i o m)
--deriving instance MonadReader r m => MonadReader r (EnvT i o m)

liftEnvT :: P.Server i o m r -> EnvT i o m r 
liftEnvT s = EnvT $ ReaderT $ \_ -> s 

instance PrimMonad m => PrimMonad (EnvT i o m) where

  type PrimState (EnvT i o m) = PrimState m
  
  primitive = Trans.lift . primitive


class MMonad (e i o) => MonadEnv e i o where

  lowerE :: Monad m => e () Void m r -> m r

  respondE :: Monad m => o -> e i o m i

  --viewEnv :: (s -> m (Maybe o)) -> e i o (m s) r -- e i o m (Maybe o)


instance MonadEnv EnvT i o where

  lowerE = runEnvT -- evalState (runEnvT e)

  respondE = liftEnvT . P.respond


runEnvT :: Monad m => EnvT () Void m r -> m r
runEnvT = P.runEffect . (`runReaderT` ()) . unEnvT

-------------------------------------------------------------------------------
-- | 

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

deriving instance MonadState s m => MonadState s (AgnT i o m)
deriving instance MonadReader r m => MonadReader r (AgnT i o m)


instance PrimMonad m => PrimMonad (AgnT i o m) where

  type PrimState (AgnT i o m) = PrimState m
  
  primitive = Trans.lift . primitive


class MMonad (a i o) => MonadAgent a i o where

  lowerA :: Monad m => a Void () m r -> m r

  requestA :: Monad m => i -> a i o m o


instance MonadAgent AgnT i o where

  lowerA = runAgnT

  requestA = AgnT . P.request


runAgnT :: Monad m => AgnT Void () m r -> m r
runAgnT = P.runEffect . unAgnT

-----------------------------------------------------------------------------------------
-- | 


newtype EpisodeT m r = EpisodeT { unEpisodeT :: P.Effect m r }
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

class MMonad p => MonadEpisode p a e where 

  episode
    :: Monad m 
    => MonadEnv   e i o
    => MonadAgent a i o
    => e i o m r -> a i o m r -> p m r

  run :: Monad m => p m r -> m r

instance MonadEpisode EpisodeT AgnT EnvT where
  
  episode e a = EpisodeT $ (runReaderT . unEnvT) e P.+>> unAgnT a

  run = P.runEffect . unEpisodeT



s :: EnvT Int Int (StateT Int IO) Int
s = loop where
    loop = do
       i <- Reader.ask
       liftIO $ print i 
       if i > 9
           then return i
           else State.modify (+ i) >> respondE (i + 1) >> loop --TODO looks wrong

c :: AgnT Int Int (State Int) Int
c = (State.get >>= requestA) >>= loop
  where
    loop i = (State.get >>= \j -> State.put (i+j) >> requestA (i + j + 1) ) >>= loop

ep1 :: EpisodeT (StateT Int IO) Int
ep1 = episode s $ hoist (hoist generalize) c

go1 :: StateT Int IO Int
go1 = run @_ @AgnT @EnvT ep1

ep2 :: EpisodeT (StateT Int (StateT Int IO)) Int
ep2 = episode (hoist lift s) (hoist lift $ hoist (hoist generalize) c)

go2 :: StateT Int (StateT Int IO) Int
go2 = run @_ @AgnT @EnvT ep2


{-





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












