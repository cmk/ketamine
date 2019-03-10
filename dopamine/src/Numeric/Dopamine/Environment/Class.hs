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

  lowerEnv :: Monad m => e () Void m r -> m r

  respondEnv :: Monad m => o -> e i o m i

  bindEnv :: Monad m => e x y m r -> (y -> e i o m x) -> e i o m r
  --viewEnv :: (s -> m (Maybe o)) -> e i o (m s) r -- e i o m (Maybe o)

respond :: (MonadEnv e i o, Monad m) => o -> e i o m i
respond = respondEnv

instance MonadEnv EnvT i o where

  lowerEnv = runEnvT -- evalState (runEnvT e)

  respondEnv = EnvT . P.respond

  bindEnv e f = EnvT $ unEnvT e P.//> unEnvT . f

{-
infixl 3 //>
infixr 3 <\\      -- GHC will raise a parse error if either of these lines ends
infixr 4 />/, >\\ -- with '\', which is why this comment is here
infixl 4 \<\, //<
infixl 5 \>\      -- Same thing here
infixr 5 /</
infixl 6 <<+
infixr 6 +>>
infixl 7 >+>, >>~
infixr 7 <+<, ~<<
infixl 8 <~<
infixr 8 >~>


-- | Equivalent to ('\>\') with the arguments flipped
(/</)
    :: Monad m
    => (c' -> Proxy b' b x' x m c)
    -- ^
    -> (b' -> Proxy a' a x' x m b)
    -- ^
    -> (c' -> Proxy a' a x' x m c)
    -- ^
p1 /</ p2 = p2 \>\ p1
{-# INLINABLE (/</) #-}

-}

infixl 3 //>
(//>) :: MonadEnv e i o => Monad m => e x y m r -> (y -> e i o m x) -> e i o m r
e //> f = bindEnv e f
{-# INLINABLE (//>) #-}

-- p1 \<\ p2 = p2 />/ p1
infixr 4 />/
(/>/) 
  :: MonadEnv e i o
  => Monad m
  => (x -> e z y m r)
  -> (y -> e i o m z) 
  ->  x -> e i o m r
fa />/ fb = \a -> fa a //> fb
{-# INLINABLE (/>/) #-}

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

  bindAgent :: Monad m => a x y m r -> (x -> a i o m y) -> a i o m r

  lowerAgent :: Monad m => a Void () m r -> m r

  requestAgent :: Monad m => i -> a i o m o


instance MonadAgent AgnT i o where

  bindAgent a f = AgnT $ unAgnT a P.//< unAgnT . f

  lowerAgent = runAgnT

  requestAgent = AgnT . P.request


request :: (MonadAgent a i o, Monad m) => i -> a i o m o
request = requestAgent

infixl 4 //<
(//<) :: MonadAgent a i o => Monad m => a x y m r -> (x -> a i o m y) -> a i o m r
a //< f = bindAgent a f
{-# INLINABLE (//<) #-}

infixr 4 >\\ 
(>\\) :: MonadAgent a i o => Monad m => (x -> a i o m y) -> a x y m r -> a i o m r
f >\\ a = a //< f
{-# INLINABLE (>\\) #-}

--fa />/ fb = \a -> fa a //< fb

{-
-- p1 \<\ p2 = p2 />/ p1
infixr 4 />/
(/>/) 
  :: MonadEnv e i o
  => Monad m
  => (x -> e z y m r)
  -> (y -> e i o m z) 
  ->  x -> e i o m r
fa />/ fb = \a -> fa a //> fb
{-# INLINABLE (/>/) #-}

-- | Equivalent to ('/>/') with the arguments flipped
(\<\)
    :: Monad m
    => (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x b' b m a')
    -- ^
    -> (a -> Proxy x' x c' c m a')
    -- ^
p1 \<\ p2 = p2 />/ p1
{-# INLINABLE (\<\) #-}


-- p1 /</ p2 = p2 \>\ p1
h :: Monad m =>
     (a1 -> AgnT a2 b m r) -> (a2 -> AgnT i o m b) -> a1 -> AgnT i o m r
h a1 a2 = AgnT . (unAgnT . a1 P./</ unAgnT . a2)


infixl 3 //>
infixr 3 <\\      -- GHC will raise a parse error if either of these lines ends
infixr 4 />/, >\\ -- with '\', which is why this comment is here
infixl 4 \<\, //<
infixl 5 \>\      -- Same thing here
infixr 5 /</
infixl 6 <<+
infixr 6 +>>
infixl 7 >+>, >>~
infixr 7 <+<, ~<<
infixl 8 <~<
infixr 8 >~>

(\>\)
    :: Monad m
    => (b' -> Proxy a' a y' y m b)
    -- ^
    -> (c' -> Proxy b' b y' y m c)
    -- ^
    -> (c' -> Proxy a' a y' y m c)
    -- ^
(fb' \>\ fc') c' = fb' >\\ fc' c'
{-# INLINABLE (\>\) #-}

{-| @(f >\\\\ p)@ replaces each 'request' in @p@ with @f@.

    Point-ful version of ('\>\')
-}
(>\\)
    :: Monad m
    => (b' -> Proxy a' a y' y m b)
    -- ^
    ->        Proxy b' b y' y m c
    -- ^
    ->        Proxy a' a y' y m c
    -- ^
fb' >\\ p0 = go p0

infixr 5 /</
(/</)
  :: MonadAgent a i o
  => Monad m
  => ( x -> a y z m r ) 
  -> ( y -> a i o m z ) 
  ->   x -> a i o m r
a1 /</ a2 = composeAgent a1 a2 
-}






--instance MonadAgent m i o => MonadAgent (IdentityT (m i o)) i o

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

class MMonad u => MonadEpisode u a e where 

  setEpisode
    :: Monad m 
    => MonadEnv   e i o
    => MonadAgent a i o
    => (i -> e i o m r) -> a i o m r -> u m r

  runEpisode :: Monad m => u m r -> m r


--run :: forall a e m p r . (MonadEpisode u a e, Monad m) => u m r -> m r
--run = runEpisode @p @a @e @m

type Conf a e i o u m = (Monad m, MonadEnv e i o, MonadAgent a i o, MonadEpisode u a e)


infixl 7 >+>
(>+>) :: Conf a e i o u m => (i -> e i o m r) -> (o -> a i o m r) -> o -> u m r
f >+> g = \x -> f +>> g x

infixr 7 <+<
(<+<) :: Conf a e i o u m => (o -> a i o m r) -> (i -> e i o m r) -> o -> u m r
g <+< f = \x -> g x <<+ f

infixr 6 +>>
(+>>) :: Conf a e i o u m => (i -> e i o m r) -> a i o m r -> u m r 
f +>> x = setEpisode f x

infixl 6 <<+
(<<+) :: Conf a e i o u m => a i o m r -> (i -> e i o m r) -> u m r
x <<+ f = setEpisode f x

{-
(>+>) infixl 7 Source#

:: Monad m	 
=> (i -> e i o m r)	 
-> (_c' -> Proxy b' b c' c m r)	 
-> _c' -> Proxy a' a c' c m r
-}

f 
  :: Monad m 
  => (i -> EnvT i o m r)
  -> (o -> AgnT i o m r) 
  ->  o -> EpisodeT m r
f e a = EpisodeT . (unEnvT . e P.>+> unAgnT . a)



instance MonadEpisode EpisodeT AgnT EnvT where
  
  setEpisode e a = EpisodeT $ unEnvT . e P.+>> unAgnT a

  runEpisode = P.runEffect . unEpisodeT

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
ep2 = sep en ag

sep
  :: Conf a e i o u m
  => Monad (s m)
  => Monad (t m)
  => Monad (s (t m))
  => MonadTrans s
  => MonadTrans t
  => MFunctor   s
  => (i -> e i o (t m) r) -> a i o (s m) r -> u (s (t m)) r
sep en ag = setEpisode (\i -> hoist lift $ en i) (hoist (hoist lift) ag)


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













