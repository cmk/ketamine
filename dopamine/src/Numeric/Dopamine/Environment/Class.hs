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
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Control.Monad.Trans.Identity (IdentityT(..), mapIdentityT)
import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Resource (MonadResource(..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Default
import Data.Functor.Identity
import Data.Maybe (maybe)

import Numeric.Dopamine.Exception (EpisodeCompleted(..))

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.State.Class as State 
import qualified Control.Monad.Trans.Class as Trans



newtype EnvT o m a = EnvT { unEnvT :: ContT o m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    --, MonadCatch
    , MonadCont
    , MonadIO
    --, MonadResource
    , MonadThrow
    , MonadTrans
    )

runEnvT :: EnvT o m a -> (a -> m o) -> m o
runEnvT = runContT . unEnvT

instance MonadState s m => MonadState s (EnvT o m) where

  state = EnvT . State.state


instance PrimMonad m => PrimMonad (EnvT o m) where

  type PrimState (EnvT o m) = PrimState m
  
  primitive = Trans.lift . primitive


class (Monad m, MonadTrans e) => MonadEnv s o m e | m -> s, e -> o where

  lowerEnv :: e m (Maybe o) -> m (Maybe o)

  viewEnv :: (s -> m (Maybe o)) -> e m (Maybe o)

  withEnv :: ((b -> m (Maybe o)) -> a -> m (Maybe o)) -> e m a -> e m b


instance MonadState s m => MonadEnv s o m (EnvT (Maybe o)) where

  lowerEnv e = runEnvT e return

  viewEnv v = Trans.lift $ State.get >>= v

  withEnv f e = EnvT . ContT $ runEnvT e . f


{-
 -
local' :: MonadEnv s o m e => (t -> m o -> m o) -> t -> e m a -> e m a
local' f g = withEnv (f g .)

local
  :: Monad m 
  => m s
  -> ((s -> s) -> m o -> m o)
  -> (s -> s) -> EnvT o m a -> EnvT o m a
local s f g (EnvT c) = EnvT $ liftLocal a f g c

-}












