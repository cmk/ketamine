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

module Numeric.Dopamine.Agent where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO(..)) 
import Control.Monad.Morph (MFunctor(..), MMonad(..), MonadTrans(..)) 
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Void (Void)

import qualified Control.Monad.Trans.Class as Trans
import qualified Pipes.Core as P


-------------------------------------------------------------------------------
-- | 


-- Agt 
newtype AgT a o m r = AgT { unAgT :: P.Proxy a o () P.X m r }
  deriving  ( Functor
            , Applicative
            , MFunctor
            , MMonad
            , Monad
            , MonadCatch
            , MonadIO
            , MonadThrow
            , MonadTrans )

deriving instance MonadState s m => MonadState s (AgT a o m)
deriving instance MonadReader r m => MonadReader r (AgT a o m)
deriving instance MonadWriter w m => MonadWriter w (AgT a o m)


instance PrimMonad m => PrimMonad (AgT a o m) where

  type PrimState (AgT a o m) = PrimState m
  
  primitive = Trans.lift . primitive


runAgent :: Monad m => AgT Void () m r -> m r
runAgent = P.runEffect . unAgT

action :: Monad m => a -> AgT a o m o
action = AgT . P.request


{- $request
    The 'request' category closely corresponds to the iteratee design pattern.

    The 'request' category obeys the category laws, where 'request' is the
    identity and ('\>\') is composition:

@
-- Left identity
'request' '\>\' f = f

\-\- Right identity
f '\>\' 'request' = f

\-\- Associativity
(f '\>\' g) '\>\' h = f '\>\' (g '\>\' h)
@

-}


infixl 4 //<

-- | 'AgT' analog of '\\>'
(//<) 
  :: Monad m => AgT x y m r -> (x -> AgT a o m y) -> AgT a o m r
a //< f = AgT $ unAgT a P.//< unAgT . f 
{-# INLINABLE (//<) #-}


infixr 4 >\\ --
 
-- | 'AgT' analog of '<\\'
(>\\) 
  :: Monad m => (x -> AgT a o m y) -> AgT x y m r -> AgT a o m r
f >\\ a = a //< f
{-# INLINABLE (>\\) #-}


infixl 5 \>\ --

-- | 'AgT' analog of '\<\'
(\>\) 
  :: Monad m => (y -> AgT a o m z) -> (x -> AgT y z m r) -> x -> AgT a o m r
fy \>\ fx = \x -> fy >\\ fx x
{-# INLINABLE (\>\) #-}


infixr 5 /</

-- | 'AgT' analog of '/>/'
(/</) 
  :: Monad m => (x -> AgT y z m r) -> (y -> AgT a o m z) -> x -> AgT a o m r
fx /</ fy = fy \>\ fx
{-# INLINABLE (/</) #-}

