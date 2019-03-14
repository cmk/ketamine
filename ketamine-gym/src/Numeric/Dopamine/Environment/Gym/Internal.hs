{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications, AllowAmbiguousTypes #-}
module Numeric.Ketamine.Environment.Gym.Internal where

import Control.Exception.Safe
import Data.DList
import Data.Aeson
import Control.Monad.Except
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import Control.Monad.Reader.Class
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.IO.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.RWS (RWST, execRWST,runRWST)
import Control.Monad.Fail (MonadFail(..))

import Servant.Client (BaseUrl, ServantError, Scheme(Http), ClientM, ClientEnv)
import System.Random.MWC (GenIO, Variate)


import qualified Data.Text as T (pack)
import qualified OpenAI.Gym as Gym
import qualified Servant.Client as C
import qualified System.Random.MWC as MWC

import OpenAI.Gym (GymEnv(..), InstID, Observation(..))

-- | A convenience type constraint with MonadMWCRandom and MonadIO.
type MonadMWCRandomIO m = (MonadIO m, MonadMWCRandom m)

data Event r o a = Event Integer r o a
  deriving Show

type Obs r o = (r, o)

type Reward = Double
data Initial o = Initial !o | EmptyEpisode

class Monad m => MonadMWCRandom m where
  getGen :: m GenIO

-- TODO: switch to my random
-- | in the end, we can always use IO to get our generator, but we will create a
-- new generator on each use.
instance MonadMWCRandom IO where
  getGen :: IO GenIO
  getGen = MWC.createSystemRandom

-- | A newtype wrapper around Servant so that 'GymT' doesn't require
-- Servant to be at the bottom of the stack
newtype ClientT t a
  = ClientT (ReaderT ClientEnv (ExceptT ServantError t) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ServantError
    , MonadReader ClientEnv
    , MonadIO
    , MonadFail
    , MonadThrow
    )


instance MonadTrans ClientT where lift = ClientT . lift . lift

-- | runner of a ClientT, which should really be in Servant, but since it's not
-- there we will roll our own. This allows us to have other things at the botton
-- of our transformer stack
runClientT :: MonadIO m => ClientT m a -> C.ClientEnv -> m (Either ServantError a)
runClientT (ClientT m) env = runExceptT $ runReaderT m env

-- | Lift a ClientM action into ClientT
liftClientM :: MonadIO m => C.ClientM a -> ClientT m a
liftClientM m = ClientT . ReaderT $ \env -> ExceptT $ liftIO (C.runClientM m env)

--type Gym a o m r = E.EnT a o (RWST GymConfigs (DList (Event Reward o a)) (LastState o) (ClientT m)) r
--TODO move to ReaderIO w/ logger cap
newtype GymT a o m r = GymT
  { unGymT :: RWST GymConfigs (DList (Event Reward o a)) (LastState o) (ClientT m) r }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadThrow
    , MonadReader GymConfigs
    , MonadWriter (DList (Event Reward o a))
    , MonadState (LastState o)
    , MonadRWS GymConfigs (DList (Event Reward o a)) (LastState o)
    )

instance MonadTrans (GymT a o) where
   lift = GymT . lift . lift


instance (MonadIO m, MonadMWCRandom m) => MonadMWCRandom (GymT a o m) where
  getGen = liftIO getGen

-- | run a servant action in 'GymT'
runInGym :: MonadIO m => ClientM x -> GymT a o m x
runInGym c = GymT $ lift $ liftClientM c

-- ========================================================================= --

-- | Configurations for an environment
-- TODO: there are more of these in the gym documentation
data GymConfigs
    = GymConfigs InstID Bool    -- ^ the instance id, as well as a flag of if we want to render the state

-- | Remember the last state of the episode
data LastState o
    = LastState Integer o       -- ^ the episode number and last state
    | Uninitialized Integer     -- ^ a flag that the state is no longer initialized, and the current episode
    deriving (Eq, Show)


-- | Possible errors we might encounter while interacting with our environment
data GymException
    = UnexpectedServerResponse String
    | TypeError String
    | EnvironmentRequiresReset
    deriving (Show)

instance Exception GymException where


-- TODO: move this into a logging monad so that we don't blow up memory.
runGym
  :: forall a o m r . MonadIO m
  => GymEnv
  -> Manager
  -> BaseUrl
  -> Bool  -- monitor flag
  -> GymT a o m r
  -> m (Either ServantError (DList (Event Reward o a)))
runGym t m u mon env = runClientT action (C.ClientEnv m u Nothing)
  where
    action :: ClientT m (DList (Event Reward o a))
    action = do
        i <- liftClientM $ Gym.envCreate t
        (_, w) <- execRWST (unGymT renderableEnv) (GymConfigs i mon) (Uninitialized 0)
        liftClientM $ Gym.envClose i
        return w

    renderableEnv :: GymT a o m ()
    renderableEnv =
        if mon
        then withMonitor env
        else void env


-- | same as 'runGym', however use http-client's default manager settings
runGymDefault
  :: MonadIO m
  => GymEnv
  -> Bool
  -> GymT a o m r
  -> m (Either ServantError (DList (Event Reward o a)))
runGymDefault t m e = do
    mngr <- liftIO (newManager defaultManagerSettings)
    runGym t mngr (C.BaseUrl Http "localhost" 5000 "") m e



-- | get the environment's id from the gym server
getInstID :: Monad m => GymT a o m InstID
getInstID = ask >>= \(GymConfigs i _) -> pure i


-- | if a user wants to render the agent as it performs, this makes sure to properly
-- start and stop the monitor
withMonitor :: MonadIO m => GymT a o m r -> GymT a o m ()
withMonitor env = do
    i <- getInstID
    runInGym $ Gym.envMonitorStart i (m i)
    _ <- env
    runInGym $ Gym.envMonitorClose i
    return ()
  where
    m :: InstID -> Gym.Monitor
    m (Gym.InstID t) = Gym.Monitor ("/tmp/"<> T.pack (show CartPoleV0) <>"-" <> t) True False False


-- | ensure that the gym has reset before stepping, otherwise throw 'EnvironmentRequiresReset'
stepCheck :: MonadThrow m => GymT a o m ()
stepCheck =
  get >>= \case
    Uninitialized _ -> throwM EnvironmentRequiresReset
    LastState _ _   -> return ()


-- | generic rest function which makes a call to the gym and returns the first observation
reset :: (MonadIO m, MonadThrow m, FromJSON o) => GymT a o m o 
reset = do
    i <- getInstID
    Observation o <- runInGym . Gym.envReset $ i
    o' <- aesonToState o
    get >>= \case
        Uninitialized ep -> put $ LastState (ep+1) o'
        LastState   ep _ -> put $ LastState (ep+1) o'
    return $ o'

-- | generic step function which takes a ToJSONable action and returns a reward
-- and a FromJSONable state
step
  :: forall a o m. (MonadIO m, MonadFail m, MonadThrow m, ToJSON a, FromJSON o)
  => a -> GymT a Value m (Maybe (Obs Reward Value))
step a = do
    stepCheck
    GymConfigs i mon <- ask
    out <- runInGym . Gym.envStep i $ renderStep mon
    let r = Gym.reward out
    if Gym.done out
    then do
        _ <- aesonToMaybeState @o $ Gym.observation out
        LastState ep prior <- get
        tell . pure $ Event ep r prior a
        return Nothing
    else do
        o' <- aesonToState $ Gym.observation out
        LastState ep prior <- get
        put $ LastState ep o'
        tell . pure $ Event ep r prior a
        return $ Just (r,o')
    where
        renderStep :: Bool -> Gym.Step
        renderStep = Gym.Step (toJSON a)

-- | Convert an aeson value into an environment's state
aesonToState :: forall o m . (FromJSON o, MonadThrow m) => Value -> m o
aesonToState = aesonToMaybeState >=> \case
    Nothing -> throw $ UnexpectedServerResponse "observation returned was null"
    Just o -> pure o


-- | Convert an aeson value into an environment's state, but safer
aesonToMaybeState :: forall o m . (FromJSON o, MonadThrow m) => Value -> m (Maybe o)
aesonToMaybeState Null = pure Nothing
aesonToMaybeState    o =
  case (fromJSON @o o) of
    Error  str -> throw $ UnexpectedServerResponse str
    Success o' -> pure  $ Just o'

