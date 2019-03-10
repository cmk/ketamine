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

import Numeric.Dopamine.Exception (EpisodeCompleted(..))

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Reader.Class as R
import qualified Control.Monad.State.Class as S 
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

runEnvT :: Monad m => EnvT () Void m r -> m r
runEnvT = P.runEffect . unEnvT


instance PrimMonad m => PrimMonad (EnvT i o m) where

  type PrimState (EnvT i o m) = PrimState m
  
  primitive = Trans.lift . primitive


class MMonad (e i o) => MonadEnv e i o where

  bindEnv :: Monad m => e x y m r -> (y -> e i o m x) -> e i o m r

  lowerEnv :: Monad m => e () Void m r -> m r

  respondEnv :: Monad m => o -> e i o m i

  --viewEnv :: (s -> m (Maybe o)) -> e i o (m s) r -- e i o m (Maybe o)

respond :: (MonadEnv e i o, Monad m) => o -> e i o m i
respond = respondEnv

instance MonadEnv EnvT i o where

  bindEnv e f = EnvT $ unEnvT e P.//> unEnvT . f

  lowerEnv = runEnvT -- evalState (runEnvT e)

  respondEnv = EnvT . P.respond



infixl 3 //>

-- | 'MonadEnv' analog of '//<'
(//>) :: MonadEnv e i o => Monad m => e x y m r -> (y -> e i o m x) -> e i o m r
e //> f = bindEnv e f
{-# INLINABLE (//>) #-}


infixl 3 <\\

-- | 'MonadEnv' analog of '>\\'
(<\\) :: MonadEnv e i o => Monad m => (y -> e i o m x) -> e x y m r -> e i o m r
f <\\ e = e //> f 
{-# INLINABLE (<\\) #-}


infixr 4 />/ --

-- | 'MonadEnv' analog of '/</'
(/>/) 
  :: MonadEnv e i o => Monad m => (x -> e z y m r) -> (y -> e i o m z) -> x -> e i o m r
fx />/ fy = \x -> fx x //> fy
{-# INLINABLE (/>/) #-}


infixl 4 \<\

-- | 'MonadEnv' analog of '\>\'
(\<\) 
  :: MonadEnv e i o => Monad m => (y -> e i o m z) -> (x -> e z y m r) -> x -> e i o m r
fy \<\ fx = fx />/ fy 
{-# INLINABLE (\<\) #-}


-----------------------------------------------------------------------------------------
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


--instance MonadAgent m i o => MonadAgent (IdentityT (m i o)) i o
runAgnT :: Monad m => AgnT Void () m r -> m r
runAgnT = P.runEffect . unAgnT


instance PrimMonad m => PrimMonad (AgnT i o m) where

  type PrimState (AgnT i o m) = PrimState m
  
  primitive = Trans.lift . primitive


class MMonad (a i o) => MonadAgent a i o where

  bindAgent :: Monad m => a x y m r -> (x -> a i o m y) -> a i o m r

  lowerAgent :: Monad m => a Void () m r -> m r

  requestAgent :: Monad m => i -> a i o m o


instance MonadAgent AgnT i o where

  bindAgent a f = AgnT $ unAgnT a P.//< unAgnT . f

  lowerAgent = runAgnT

  requestAgent = AgnT . P.request


request :: (MonadAgent a i o, Monad m) => i -> a i o m o
request = requestAgent



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
  :: MonadAgent a i o => Monad m => a x y m r -> (x -> a i o m y) -> a i o m r
a //< f = bindAgent a f
{-# INLINABLE (//<) #-}


infixr 4 >\\ --
 
-- | 'MonadAgent' equivalent of '<\\'
(>\\) 
  :: MonadAgent a i o => Monad m => (x -> a i o m y) -> a x y m r -> a i o m r
f >\\ a = a //< f
{-# INLINABLE (>\\) #-}


infixl 5 \>\ --

-- | 'MonadAgent' analog of '\<\'
(\>\) 
  :: MonadAgent a i o => Monad m => (y -> a i o m z) -> (x -> a y z m r) -> x -> a i o m r
fy \>\ fx = \x -> fy >\\ fx x
{-# INLINABLE (\>\) #-}


infixr 5 /</

-- | 'MonadAgent' analog of '/>/'
(/</) 
  :: MonadAgent a i o => Monad m => (x -> a y z m r) -> (y -> a i o m z) -> x -> a i o m r
fx /</ fy = fy \>\ fx
{-# INLINABLE (/</) #-}





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

class MMonad u => MonadEpisode u a e where 



  withAgent 
    :: Monad m 
    => MonadEnv   e i o
    => MonadAgent a i o
    => a i o m r -> (i -> e i o m r) -> u m r

  runEpisode :: Monad m => u m r -> m r

instance MonadEpisode EpisodeT AgnT EnvT where
  
  withAgent a e = EpisodeT $ unAgnT a P.<<+ unEnvT . e

  runEpisode = P.runEffect . unEpisodeT


--run :: forall a e m p r . (MonadEpisode u a e, Monad m) => u m r -> m r
--run = runEpisode @p @a @e @m

type Ep a e i o u m = (Monad m, MonadEnv e i o, MonadAgent a i o, MonadEpisode u a e)

{-| Compose two episodes blocked in the middle of 'respond'ing, creating a new
    episode blocked in the middle of 'respond'ing

@
(f '>+>' g) x = f '+>>' g x
@

-}

infixr 6 +>>
(+>>) :: Ep a e i o u m => (i -> e i o m r) -> a i o m r -> u m r 
f +>> x = withAgent x f
{-# INLINABLE [1] (+>>) #-}


infixl 6 <<+
(<<+) :: Ep a e i o u m => a i o m r -> (i -> e i o m r) -> u m r
x <<+ f = f +>> x


infixl 7 >+>
(>+>) :: Ep a e i o u m => (i -> e i o m r) -> (o -> a i o m r) -> o -> u m r
f >+> g = \x -> f +>> g x
{-# INLINABLE (>+>) #-}


infixr 7 <+<
(<+<) :: Ep a e i o u m => (o -> a i o m r) -> (i -> e i o m r) -> o -> u m r
g <+< f = f >+> g


reflectEnv :: Monad m => EnvT i o m r -> AgnT o i m r
reflectEnv = AgnT . P.reflect . unEnvT


reflectAgent :: Monad m => AgnT i o m r -> EnvT o i m r
reflectAgent = EnvT . P.reflect . unAgnT





withAgent'
  :: Ep a e i o u m
  => Monad (s m)
  => Monad (t m)
  => Monad (s (t m))
  => MonadTrans s
  => MonadTrans t
  => MFunctor   s
  => a i o (s m) r -> (i -> e i o (t m) r) -> u (s (t m)) r
withAgent' ag en = withAgent (hoist (hoist lift) ag) (\i -> hoist lift $ en i) 


-- TODO move to readme / put in lhs file
-- TODO give example using ether
en :: Int -> EnvT Int Int (ReaderT (IORef Int) IO) ()
en = loop where
    loop = \i -> do
         r <- lift ask
         j <- liftIO $ readIORef r
         liftIO $ print $ "en state: " ++ show j
         liftIO $ modifyIORef r $ \k -> k - 1
         if i > 4
             then return ()
             else respond i >>= loop

ag :: AgnT Int Int (ReaderT (IORef Int) IO) ()
ag = request 1 >>= loop where
    loop _ = do
         r <- lift ask
         j <- liftIO $ readIORef r
         liftIO $ print $ "ag state: " ++ show j
         liftIO $ modifyIORef r $ \k -> k + 2
         request j >>= loop

ep1 :: EpisodeT (ReaderT (IORef Int) IO) ()
ep1 = en +>> ag

test1 :: IO ()
test1 = do
    agr <- newIORef (1 :: Int)
    let rt = runEpisode @_ @AgnT @EnvT $ ep1
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



ep2 :: EpisodeT (ReaderT (IORef Int) (ReaderT (IORef Int) IO)) ()
--ep2 = episode (\i -> hoist lift $ en i) (hoist (hoist lift) ag)
ep2 = withAgent' ag en


test2 :: IO ()
test2 = do
    agr <- newIORef (1 :: Int)
    enr <- newIORef (1 :: Int)
    let rt = runEpisode @_ @AgnT @EnvT $ ep2 
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













