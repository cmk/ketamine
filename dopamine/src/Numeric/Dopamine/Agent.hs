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

module Numeric.Dopamine.Agent where

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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Select
import Data.Bool (bool)
import Data.Default
import Data.Functor.Identity
import Data.Maybe

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.State.Class as State 
import qualified Control.Monad.Trans.Class as Trans
import qualified System.Random.MWC as R




newtype AgentT o m a = AgnetT { unAgentT :: SelectT o m a }
  deriving (Functor, Applicative, Monad)

runAgentT :: AgentT o m a -> (a -> m o) -> m a
runAgentT = runSelectT . unAgentT
