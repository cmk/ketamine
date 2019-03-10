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
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
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
import Data.IORef
import Pipes.Core (Proxy(..), Server, Client, Effect)

import Numeric.Dopamine.Exception (EpisodeCompleted(..))

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Reader.Class as R
import qualified Control.Monad.State.Class as S 
import qualified Control.Monad.Trans.Class as Trans
import qualified Pipes.Core as P





--(>\\) = P.(>\\)
-- EnT
-- -- tic tac toe ex
-- leave EnT and AgT together
newtype EnT i o m r = EnT { unEnT :: P.Server i o m r }
  deriving  ( Functor
            , Applicative
            , MFunctor
            , MMonad
            , Monad
            , MonadCatch
            , MonadIO
            , MonadThrow
            , MonadTrans )

deriving instance MonadState s m => MonadState s (EnT i o m)
deriving instance MonadReader r m => MonadReader r (EnT i o m)


instance PrimMonad m => PrimMonad (EnT i o m) where

  type PrimState (EnT i o m) = PrimState m
  
  primitive = Trans.lift . primitive

runEnT :: Monad m => EnT () Void m r -> m r
runEnT = P.runEffect . unEnT

respond :: Monad m => o -> EnT i o m i
respond = EnT . P.respond

bindEnv :: Monad m => EnT x y m r -> (y -> EnT i o m x) -> EnT i o m r
bindEnv e f = EnT $ unEnT e P.//> unEnT . f



infixl 3 //>

-- | 'MonadEnv' analog of '//<'
(//>) :: Monad m => EnT x y m r -> (y -> EnT i o m x) -> EnT i o m r
e //> f = bindEnv e f
{-# INLINABLE (//>) #-}


infixr 3 <\\

-- | 'MonadEnv' analog of '>\\'
(<\\) :: Monad m => (y -> EnT i o m x) -> EnT x y m r -> EnT i o m r
f <\\ e = e //> f 
{-# INLINABLE (<\\) #-}


infixr 4 />/ --

-- | 'MonadEnv' analog of '/</'
(/>/) 
  :: Monad m => (x -> EnT z y m r) -> (y -> EnT i o m z) -> x -> EnT i o m r
fx />/ fy = \x -> fx x //> fy
{-# INLINABLE (/>/) #-}


infixl 4 \<\

-- | 'MonadEnv' analog of '\>\'
(\<\) 
  :: Monad m => (y -> EnT i o m z) -> (x -> EnT z y m r) -> x -> EnT i o m r
fy \<\ fx = fx />/ fy 
{-# INLINABLE (\<\) #-}


-----------------------------------------------------------------------------------------
-- | 

-- Agt 
newtype AgT i o m r = AgT { unAgT :: P.Client i o m r }
  deriving  ( Functor
            , Applicative
            , MFunctor
            , MMonad
            , Monad
            , MonadCatch
            , MonadIO
            , MonadThrow
            , MonadTrans )

deriving instance MonadState s m => MonadState s (AgT i o m)
deriving instance MonadReader r m => MonadReader r (AgT i o m)


instance PrimMonad m => PrimMonad (AgT i o m) where

  type PrimState (AgT i o m) = PrimState m
  
  primitive = Trans.lift . primitive


runAgT :: Monad m => AgT Void () m r -> m r
runAgT = P.runEffect . unAgT

bindAgent :: Monad m => AgT x y m r -> (x -> AgT i o m y) -> AgT i o m r
bindAgent a f = AgT $ unAgT a P.//< unAgT . f

request :: Monad m => i -> AgT i o m o
request = AgT . P.request


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

(//<) 
  :: Monad m => AgT x y m r -> (x -> AgT i o m y) -> AgT i o m r
a //< f = bindAgent a f
{-# INLINABLE (//<) #-}


infixr 4 >\\ --
 
-- | 'MonadAgent' equivalent of '<\\'
(>\\) 
  :: Monad m => (x -> AgT i o m y) -> AgT x y m r -> AgT i o m r
f >\\ a = a //< f
{-# INLINABLE (>\\) #-}


infixl 5 \>\ --

-- | 'MonadAgent' analog of '\<\'
(\>\) 
  :: Monad m => (y -> AgT i o m z) -> (x -> AgT y z m r) -> x -> AgT i o m r
fy \>\ fx = \x -> fy >\\ fx x
{-# INLINABLE (\>\) #-}


infixr 5 /</

-- | 'MonadAgent' analog of '/>/'
(/</) 
  :: Monad m => (x -> AgT y z m r) -> (y -> AgT i o m z) -> x -> AgT i o m r
fx /</ fy = fy \>\ fx
{-# INLINABLE (/</) #-}





-----------------------------------------------------------------------------------------
-- | 


newtype EpT m r = EpT { unEpT :: P.Effect m r }
  deriving  ( Functor
            , Applicative
            , MFunctor
            , MMonad
            , Monad
            , MonadCatch
            , MonadIO
            , MonadThrow
            , MonadTrans )

{-
class MMonad EpT => MonadEpisode EpT a e where 

  
  
  -}

--instance MonadEpisode EpT AgT EnT where
 
withAgent :: Monad m => AgT i o m r -> (i -> EnT i o m r) -> EpT m r 
withAgent a f = EpT $ unAgT a P.<<+ unEnT . f

runEpisode :: Monad m => EpT m r -> m r
runEpisode = P.runEffect . unEpT

withEnvironment :: Monad m => EnT i o m r -> (o -> AgT i o m r) -> EpT m r
withEnvironment e f = EpT $ unEnT e P.>>~ unAgT . f


--type Monad m = (Monad m, MonadAgent a i o, MonadEpisode EpT a e)

type Conf s t m = 
  (Monad (s m), Monad (t m), Monad (s (t m)), MonadTrans s, MonadTrans t, MFunctor s)


below :: (MFunctor t1, MFunctor t2, MFunctor t3, Monad m1, Monad m2, Monad (t3 m2), MonadTrans t4, MonadTrans t5) 
   => (t1 (t4 m1) b1 -> (a -> t2 (t3 (t5 m2)) b2) -> t6)
   -> t1 m1 b1 -> (a -> t2 (t3 m2) b2) -> t6
below k x y = k (hoist lift x) $ hoist (hoist lift) . y


above  :: (MFunctor t1, MFunctor t2, MFunctor t3, Monad m1, Monad m2, Monad (t3 m2), MonadTrans t4, MonadTrans t5) 
  => (t1 (t3 (t4 m2)) b1 -> (a -> t2 (t5 m1) b2) -> t6)
  -> t1 (t3 m2) b1 -> (a -> t2 m1 b2) -> t6
above k x y = k (hoist (hoist lift) x) $ hoist lift . y

{-
foo a e = unAgT . a P.>~> unEnT . e

\ a e -> EpT $ unAgT . a P.<~< unEnT . e
  :: Monad m =>
     (o -> AgT i o m r)
     -> (() -> EnT i o m r) -> () -> EpT m r
infixl 7 >+>, >>~
infixr 7 <+<, ~<<
infixl 8 <~<
infixr 8 >~>

-}



{-| Compose two episodes blocked in the middle of 'respond'ing, creating a new
    episode blocked in the middle of 'respond'ing

@
(f '>+>' g) x = f '+>>' g x
@

-}

infixr 6 +>>

(+>>) :: Monad m 
      => (i -> EnT i o m r) -> AgT i o m r -> EpT m r 
f +>> a = withAgent a f
{-# INLINABLE [1] (+>>) #-}

infixr 6 +/>

(+/>) :: Conf s t m 
      => Monad m 
      => (i -> EnT i o (t m) r) -> AgT i o (s m) r -> EpT (s (t m)) r
f +/> a = above withAgent a f
{-# INLINABLE [1] (+/>) #-}


infixr 6 +\>

(+\>) :: Conf t s m 
      => Monad m 
      => (i -> EnT i o (t m) r) -> AgT i o (s m) r -> EpT (t (s m)) r
f +\> a = below withAgent a f
{-# INLINABLE [1] (+\>) #-}


infixl 7 >>~

(>>~) :: Monad m => EnT i o m r -> (o -> AgT i o m r) -> EpT m r
e >>~ f = withEnvironment e f


infixl 7 >/~

(>/~) :: Conf s t m
      => Monad m 
      => EnT i o (s m) r -> (o -> AgT i o (t m) r) -> EpT (s (t m)) r
e >/~ f = above withEnvironment e f

-- TODO add triples for these as well
--infixr 7 ~<<
--infixl 8 <~<
--infixr 8 >~>

infixl 7 >\~

(>\~) :: Conf t s m
      => Monad m 
      => EnT i o (s m) r -> (o -> AgT i o (t m) r) -> EpT (t (s m)) r
e >\~ f = below withEnvironment e f



infixl 6 <<+

(<<+) :: Monad m => AgT i o m r -> (i -> EnT i o m r) -> EpT m r
x <<+ f = f +>> x



infixl 6 <\+

(<\+) :: Conf s t m 
      => Monad m 
      => AgT i o (s m) r -> (i -> EnT i o (t m) r) -> EpT (s (t m)) r
x <\+ f = f +/> x
{-# INLINABLE (<\+) #-}



infixl 7 >+>

(>+>) :: Monad m => (i -> EnT i o m r) -> (o -> AgT i o m r) -> o -> EpT m r
f >+> g = \x -> f +>> g x
{-# INLINABLE (>+>) #-}


infixl 7 /+>

(/+>) :: Conf s t m 
      => Monad m 
      => (i -> EnT i o (t m) r) -> (o -> AgT i o (s m) r) -> o -> EpT (s (t m)) r
f /+> g = \x -> f +/> g x


infixr 7 <+<

(<+<) :: Monad m => (o -> AgT i o m r) -> (i -> EnT i o m r) -> o -> EpT m r
g <+< f = f >+> g


infixr 7 <+\

(<+\) :: Conf s t m 
      => Monad m 
      => (o -> AgT i o (s m) r) -> (i -> EnT i o (t m) r) -> o -> EpT (s (t m)) r
g <+\ f = f /+> g


reflectEnv :: Monad m => EnT i o m r -> AgT o i m r
reflectEnv = AgT . P.reflect . unEnT

reflectAgent :: Monad m => AgT i o m r -> EnT o i m r
reflectAgent = EnT . P.reflect . unAgT

runWithAgent
  :: Monad m 
  => AgT i o m r -> (i -> EnT i o m r) -> m r
runWithAgent a = runEpisode . withAgent a

{-
> :t pull
pull :: Monad m => a' -> Proxy a' a a' a m r
> :t push
push :: Monad m => AgT -> Proxy a' a a' a m r
-}


-- TODO move to readme / put in lhs file
-- TODO give example using ether
en :: Int -> EnT Int Int (ReaderT (IORef Int) IO) ()
en = loop where
    loop = \i -> do
         r <- lift ask
         j <- liftIO $ readIORef r
         liftIO $ print $ "en state: " ++ show j
         liftIO $ modifyIORef r $ \k -> k - 1
         if i > 4
             then return ()
             else respond i >>= loop

ag :: AgT Int Int (ReaderT (IORef Int) IO) ()
ag = request 1 >>= loop where
    loop _ = do
         r <- lift ask
         j <- liftIO $ readIORef r
         liftIO $ print $ "ag state: " ++ show j
         liftIO $ modifyIORef r $ \k -> k + 2
         request j >>= loop

test1 :: IO ()
test1 = do
    agr <- newIORef (1 :: Int)
    let rt :: ReaderT (IORef Int) IO ()
        rt = runEpisode $ en +>> ag
    runReaderT rt agr

{-
> test1
"en state: 1"
"ag state: 0"
"en state: 2"
"ag state: 1"
"en state: 3"
"ag state: 2"
"en state: 4"
"ag state: 3"
"en state: 5"
"ag state: 4"
"en state: 6"
"ag state: 5"
"en state: 7"
-}






test2 :: IO ()
test2 = do
    agr <- newIORef (1 :: Int)
    enr <- newIORef (1 :: Int)
    let rt :: ReaderT (IORef Int) (ReaderT (IORef Int) IO) ()
        rt = runEpisode $ en +/> ag 
    (`runReaderT` enr) . (`runReaderT` agr) $ rt

{-
> test2
"en state: 1"
"ag state: 1"
"en state: 0"
"ag state: 3"
"en state: -1"
"ag state: 5"
"en state: -2"
-}













