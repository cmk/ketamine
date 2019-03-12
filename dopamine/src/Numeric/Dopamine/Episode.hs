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

module Numeric.Dopamine.Episode where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO(..)) 
import Control.Monad.Morph (MFunctor(..), MMonad(..), MonadTrans(..)) 

import Numeric.Dopamine.Agent
import Numeric.Dopamine.Environment
import Numeric.Dopamine.Exception (EpisodeCompleted(..))

import qualified Pipes.Core as P



newtype EpT m r = EpT { unEpT :: P.Proxy P.X () () P.X m r }
  deriving  ( Functor
            , Applicative
            , MFunctor
            , MMonad
            , Monad
            , MonadCatch
            , MonadIO
            , MonadThrow
            , MonadTrans )

runEpisode :: Monad m => EpT m r -> m r
runEpisode = P.runEffect . unEpT

withAgent :: Monad m => AgT a o m r -> (a -> EnT a o m r) -> EpT m r 
withAgent a f = EpT $ unAgT a P.<<+ unEnT . f

runWithAgent :: Monad m => AgT a o m r -> (a -> EnT a o m r) -> m r
runWithAgent a = runEpisode . withAgent a

withEnvironment :: Monad m => EnT a o m r -> (o -> AgT a o m r) -> EpT m r
withEnvironment e f = EpT $ unEnT e P.>>~ unAgT . f

runWithEnvironment :: Monad m => EnT i o m r -> (o -> AgT i o m r) -> m r
runWithEnvironment a = runEpisode . withEnvironment a

reflectAgent :: Monad m => AgT a o m r -> EnT o a m r
reflectAgent = EnT . P.reflect . unAgT

reflectEnvironment :: Monad m => EnT a o m r -> AgT o a m r
reflectEnvironment = AgT . P.reflect . unEnT

-------------------------------------------------------------------------------
-- | 

type M s t m = 
  (Monad (s m), Monad (t m), Monad (s (t m)), MonadTrans s, MonadTrans t, MFunctor s)

below :: (MFunctor t1, MFunctor t2, MFunctor t3, Monad m1, Monad m2, Monad (t3 m2), MonadTrans t4, MonadTrans t5) 
   => (t1 (t4 m1) b1 -> (a -> t2 (t3 (t5 m2)) b2) -> t6)
   -> t1 m1 b1 -> (a -> t2 (t3 m2) b2) -> t6
below k x y = k (hoist lift x) $ hoist (hoist lift) . y

above  :: (MFunctor t1, MFunctor t2, MFunctor t3, Monad m1, Monad m2, Monad (t3 m2), MonadTrans t4, MonadTrans t5) 
  => (t1 (t3 (t4 m2)) b1 -> (a -> t2 (t5 m1) b2) -> t6)
  -> t1 (t3 m2) b1 -> (a -> t2 m1 b2) -> t6
above k x y = k (hoist (hoist lift) x) $ hoist lift . y


infixr 6 +>>

(+>>) :: Monad m 
      => (a -> EnT a o m r) -> AgT a o m r -> EpT m r 
f +>> a = withAgent a f
{-# INLINABLE [1] (+>>) #-}

infixr 6 +/>

(+/>) :: M s t m 
      => Monad m 
      => (a -> EnT a o (t m) r) -> AgT a o (s m) r -> EpT (s (t m)) r
f +/> a = above withAgent a f
{-# INLINABLE [1] (+/>) #-}


infixr 6 +\>

(+\>) :: M t s m 
      => Monad m 
      => (a -> EnT a o (t m) r) -> AgT a o (s m) r -> EpT (t (s m)) r
f +\> a = below withAgent a f
{-# INLINABLE [1] (+\>) #-}


infixl 7 >>~

(>>~) :: Monad m => EnT a o m r -> (o -> AgT a o m r) -> EpT m r
e >>~ f = withEnvironment e f


infixl 7 >/~

(>/~) :: M s t m
      => Monad m 
      => EnT a o (s m) r -> (o -> AgT a o (t m) r) -> EpT (s (t m)) r
e >/~ f = above withEnvironment e f

-- TODO add triples for these as well
--infixr 7 ~<<
--infixl 8 <~<
--infixr 8 >~>

infixl 7 >\~

(>\~) :: M t s m
      => Monad m 
      => EnT a o (s m) r -> (o -> AgT a o (t m) r) -> EpT (t (s m)) r
e >\~ f = below withEnvironment e f



infixl 6 <<+

(<<+) :: Monad m => AgT a o m r -> (a -> EnT a o m r) -> EpT m r
x <<+ f = f +>> x



infixl 6 <\+

(<\+) :: M s t m 
      => Monad m 
      => AgT a o (s m) r -> (a -> EnT a o (t m) r) -> EpT (s (t m)) r
x <\+ f = f +/> x
{-# INLINABLE (<\+) #-}


{-| Compose an outcome generator with an action generator, creating an
    episode awaiting an initial outcome state.
@
(f '>+>' g) x = f '+>>' g x
@

-}

infixl 7 >+>

(>+>) :: Monad m => (a -> EnT a o m r) -> (o -> AgT a o m r) -> o -> EpT m r
f >+> g = \x -> f +>> g x
{-# INLINABLE (>+>) #-}


infixl 7 /+>

(/+>) :: M s t m 
      => Monad m 
      => (a -> EnT a o (t m) r) -> (o -> AgT a o (s m) r) -> o -> EpT (s (t m)) r
f /+> g = \x -> f +/> g x


infixr 7 <+<

(<+<) :: Monad m => (o -> AgT a o m r) -> (a -> EnT a o m r) -> o -> EpT m r
g <+< f = f >+> g


infixr 7 <+\

(<+\) :: M s t m 
      => Monad m 
      => (o -> AgT a o (s m) r) -> (a -> EnT a o (t m) r) -> o -> EpT (s (t m)) r
g <+\ f = f /+> g





