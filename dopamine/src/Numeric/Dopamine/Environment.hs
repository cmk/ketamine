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

module Numeric.Dopamine.Environment (
  module Numeric.Dopamine.Environment.Class,
  module Numeric.Dopamine.Environment
) where


import Control.Exception.Safe 
import Control.Monad
import Control.Monad.Cont.Class 
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Default
import Data.Maybe
import Pipes.Core (Proxy(..), Server, Client, Effect)

import Numeric.Dopamine.Episode
import Numeric.Dopamine.Environment.Class
import Numeric.Dopamine.Exception (EpisodeCompleted(..))

import qualified Control.Monad.Trans.Class as Trans
import qualified Pipes.Core as P



