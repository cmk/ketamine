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
             StandaloneDeriving,
             TypeApplications
#-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Numeric.Dopamine.Environment where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO(..)) 
import Control.Monad.Morph (MFunctor(..), MMonad(..), MonadTrans(..)) 
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Void (Void)

import qualified Control.Monad.Trans.Class as Trans
import qualified Pipes.Core as P

-------------------------------------------------------------------------------
-- | 

newtype EnT a o m r = EnT { unEnT :: P.Proxy P.X () a o m r }
  deriving  ( Functor
            , Applicative
            , MFunctor
            , MMonad
            , Monad
            , MonadCatch
            , MonadIO
            , MonadThrow
            , MonadTrans )

deriving instance MonadState s m => MonadState s (EnT a o m)
deriving instance MonadReader r m => MonadReader r (EnT a o m)


instance PrimMonad m => PrimMonad (EnT a o m) where

  type PrimState (EnT a o m) = PrimState m
  
  primitive = Trans.lift . primitive


runEnvironment :: Monad m => EnT () Void m r -> m r
runEnvironment = P.runEffect . unEnT

outcome :: Monad m => o -> EnT a o m a
outcome = EnT . P.respond

bindEnvironment :: Monad m => EnT x y m r -> (y -> EnT a o m x) -> EnT a o m r
bindEnvironment e f = EnT $ unEnT e P.//> unEnT . f


infixl 3 //>

-- | 'MonadEnv' analog of '//<'
(//>) :: Monad m => EnT x y m r -> (y -> EnT a o m x) -> EnT a o m r
e //> f = bindEnvironment e f
{-# INLINABLE (//>) #-}


infixr 3 <\\

-- | 'MonadEnv' analog of '>\\'
(<\\) :: Monad m => (y -> EnT a o m x) -> EnT x y m r -> EnT a o m r
f <\\ e = e //> f 
{-# INLINABLE (<\\) #-}


infixr 4 />/ --

-- | 'MonadEnv' analog of '/</'
(/>/) 
  :: Monad m => (x -> EnT z y m r) -> (y -> EnT a o m z) -> x -> EnT a o m r
fx />/ fy = \x -> fx x //> fy
{-# INLINABLE (/>/) #-}


infixl 4 \<\

-- | 'MonadEnv' analog of '\>\'
(\<\) 
  :: Monad m => (y -> EnT a o m z) -> (x -> EnT z y m r) -> x -> EnT a o m r
fy \<\ fx = fx />/ fy 
{-# INLINABLE (\<\) #-}


