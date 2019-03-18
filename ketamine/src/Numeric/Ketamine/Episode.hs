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

module Numeric.Ketamine.Episode where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO(..)) 
import Control.Monad.Morph (MFunctor(..), MMonad(..), MonadTrans(..)) 

import Numeric.Ketamine.Agent
import Numeric.Ketamine.Environment
import Numeric.Ketamine.Util.Exception (EpisodeCompleted(..))

--import Pipes.Safe
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


-------------------------------------------------------------------------------
-- | 

infixr 6 +>>

(+>>) :: Monad m 
      => (a -> EnT a o m r) -> AgT a o m r -> EpT m r 
f +>> a = EpT $ unEnT . f P.+>> unAgT a 
{-# INLINABLE [1] (+>>) #-}


infixl 6 <<+

(<<+) :: Monad m => AgT a o m r -> (a -> EnT a o m r) -> EpT m r
a <<+ f = f +>> a


infixl 6 <\+

(<\+) :: M s t m 
      => Monad m 
      => AgT a o (s m) r -> (a -> EnT a o (t m) r) -> EpT (s (t m)) r
a <\+ f = above (<<+) a f
{-# INLINABLE (<\+) #-}


infixr 6 +/>

(+/>) :: M s t m 
      => Monad m 
      => (a -> EnT a o (t m) r) -> AgT a o (s m) r -> EpT (s (t m)) r
f +/> a = a <\+ f
{-# INLINABLE [1] (+/>) #-}


infixl 6 </+

(</+) :: M t s m 
      => Monad m 
      => AgT a o (s m) r -> (a -> EnT a o (t m) r) -> EpT (t (s m)) r
a </+ f = below (<<+) a f
{-# INLINABLE [1] (</+) #-}


infixr 6 +\>

(+\>) :: M t s m 
      => Monad m 
      => (a -> EnT a o (t m) r) -> AgT a o (s m) r -> EpT (t (s m)) r
f +\> a = a </+ f  
{-# INLINABLE [1] (+\>) #-}


-------------------------------------------------------------------------------
-- | 


infixr 7 ~<<

(~<<) :: Monad m => (o -> AgT a o m r) -> EnT a o m r -> EpT m r
f ~<< e = EpT $ unAgT . f P.~<< unEnT e


infixl 7 >>~

(>>~) :: Monad m => EnT a o m r -> (o -> AgT a o m r) -> EpT m r
e >>~ f = f ~<< e


infixl 7 >/~

(>/~) :: M s t m
      => Monad m 
      => EnT a o (s m) r -> (o -> AgT a o (t m) r) -> EpT (s (t m)) r
e >/~ f = f ~\< e


infixr 7 ~\<

(~\<) :: M s t m
      => Monad m 
      => (o -> AgT a o (t m) r) -> EnT a o (s m) r -> EpT (s (t m)) r
f ~\< e = above (>>~) e f


infixl 7 >\~

(>\~) :: M t s m
      => Monad m 
      => EnT a o (s m) r -> (o -> AgT a o (t m) r) -> EpT (t (s m)) r
e >\~ f = f ~/< e


infixr 7 ~/<

(~/<) :: M t s m
      => Monad m 
      => (o -> AgT a o (t m) r) -> EnT a o (s m) r -> EpT (t (s m)) r
f ~/< e = below (>>~) e f

-------------------------------------------------------------------------------
-- | 

{-| Compose an outcome generator with an action generator, creating an
    episode awaiting an initial outcome.
@
(f '>+>' g) o = f '+>>' g o
@

-}

infixl 7 >+>

(>+>) :: Monad m => (a -> EnT a o m r) -> (o -> AgT a o m r) -> o -> EpT m r
f >+> g = \x -> f +>> g x
{-# INLINABLE (>+>) #-}


infixr 7 <+<

(<+<) :: Monad m => (o -> AgT a o m r) -> (a -> EnT a o m r) -> o -> EpT m r
g <+< f = f >+> g


infixl 7 /+>

(/+>) :: M s t m 
      => Monad m 
      => (a -> EnT a o (t m) r) -> (o -> AgT a o (s m) r) -> o -> EpT (s (t m)) r
f /+> g = \x -> f +/> g x


infixr 7 <+\

(<+\) :: M s t m 
      => Monad m 
      => (o -> AgT a o (s m) r) -> (a -> EnT a o (t m) r) -> o -> EpT (s (t m)) r
g <+\ f = f /+> g


-------------------------------------------------------------------------------
-- | 

{-| Compose an outcome generator with an action generator, creating an
    episode awaiting an initial action.
@
(f '>~>' g) a = f a '>>~' g
@

-}

infixr 8 >~>

(>~>) :: Monad m => (a -> EnT a o m r) -> (o -> AgT a o m r) -> a -> EpT m r
f >~> g = \a -> f a >>~ g
{-# INLINABLE (>~>) #-}


infixl 8 <~<

(<~<) :: Monad m => (o -> AgT a o m r) -> (a -> EnT a o m r) -> a -> EpT m r
g <~< f = f >~> g
{-# INLINABLE (<~<) #-}


infixl 7 /~>

(/~>) :: M s t m 
      => Monad m 
      => (a -> EnT a o (s m) r) -> (o -> AgT a o (t m) r) -> a -> EpT (s (t m)) r
f /~> g = \o -> f o >/~ g


infixr 7 <~\

(<~\) :: M s t m 
      => Monad m 
      => (o -> AgT a o (t m) r) -> (a -> EnT a o (s m) r) -> a -> EpT (s (t m)) r
g <~\ f = f /~> g


infixl 7 \~>

(\~>) :: M t s m 
      => Monad m 
      => (a -> EnT a o (s m) r) -> (o -> AgT a o (t m) r) -> a -> EpT (t (s m)) r
f \~> g = \o -> f o >\~ g


infixr 7 <~/

(<~/) :: M t s m 
      => Monad m 
      => (o -> AgT a o (t m) r) -> (a -> EnT a o (s m) r) -> a -> EpT (t (s m)) r
g <~/ f = f \~> g
