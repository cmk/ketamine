{-# LANGUAGE DeriveGeneric, 
             MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts, 
             GeneralizedNewtypeDeriving,
             FunctionalDependencies, 
             UndecidableInstances, 
             TypeSynonymInstances,
             TypeFamilies 
#-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main where

import Control.Exception.Safe (assert, MonadThrow)
import Control.Monad
import Control.Monad.Morph (MFunctor(..), MMonad(..), generalize)
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.RWS.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.RWS (RWST, evalRWST)
import Data.List (maximumBy, sort)
import Data.Ord (comparing)
import Data.Vector ((!), Vector)

import qualified Control.Monad.Trans.Reader as TR
import qualified Data.DList as D
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Statistics.Distribution as Dist
import qualified Statistics.Distribution.Normal as N
import qualified System.Random.MWC as R

import Numeric.Dopamine.Environment hiding (stepEnv)
import qualified Numeric.Dopamine.Outcome as O


newtype Environment a = 
  Environment { getEnvironment :: RWST EnvState (D.DList Outcome) BanditState IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader EnvState
    , MonadWriter (D.DList Outcome)
    , MonadState BanditState
    , MonadRWS EnvState (D.DList Outcome) BanditState
    )

instance PrimMonad Environment where
  type PrimState Environment = RealWorld
  primitive = Environment . primitive


--get = liftIO . readIORef =<< ask
--put s = liftIO . (`writeIORef` s) =<< ask

-- | The slot machine index whose arm will be pulled
type Action = Int
type Reward = Double
type Outcome = O.Outcome Action Double

--type BanditEnv = EnvT Outcome (ReaderT EnvState IO)
type BanditEnv = EnvT Outcome Environment

-- | Default config of a n-armed bandit
defaultEnvState :: Int -> IO EnvState
defaultEnvState n = mkEnvState n $ replicate n 1.0

episodeLength :: Int
episodeLength = 20

main :: IO ()
main = do
  config <- defaultEnvState 10
  res <- runEnvironment config act
  mapM_ print $ D.toList res
  print config

askEnv :: MonadEnv EnvState Int m e => e m Int
askEnv = view $ \s -> 
  if pulls s <= episodeLength then Just $ pulls s else Nothing

respond :: MonadEnv s Outcome Environment e => e Environment Action -> Environment Outcome
respond = step stepEnv 

--banditView :: BanditEnv s
--banditView = do { s <- get; view' (const True) }

--step stepEnv :: MonadEnv s Outcome Environment e => e Environment Action -> Environment Outcome

-- | Run an n-armed bandit environment
runEnvironment :: EnvState -> Environment a -> IO (D.DList Outcome)
runEnvironment c (Environment m) = snd <$> evalRWST (i >> m) c Map.empty
  where (Environment i) = mkBandit

------------------------------------------------------------------------------

data EnvState = 
  EnvState { arms :: Vector N.NormalDistribution , gen :: R.GenIO, pulls :: Int }

instance Show EnvState where
  show c = "EnvState" ++
    "{ means = " ++ show (fmap Dist.mean . V.toList $ arms c) ++ ", pulls = " ++ show (pulls c) ++ " }"

mkEnvState :: Int -> [Double] -> IO EnvState
mkEnvState n vars = do
  gen <- R.createSystemRandom
  means <- replicateM n $ R.uniform gen
  let arms = V.fromList $ zipWith N.normalDistr (reverse $ sort means) vars
  return $ EnvState arms gen 0

mkBandit :: Environment BanditState
mkBandit = do
  n <- reader $ V.length . arms
  let init = Map.fromList $ take n $ zip [0..] (repeat mempty)
  put init
  return init

stepEnv :: Action -> Environment Outcome
stepEnv action = do
  rwd <- genContVar =<< (! action) . arms <$> ask
  modify $ addStats action (Stats action rwd)
  s <- get
  tell . pure $ O.Outcome action rwd
  return $ O.Outcome action rwd

genContVar :: Dist.ContGen d => d -> Environment Double
genContVar d = do
  g <- reader gen
  liftIO $ Dist.genContVar d g

------------------------------------------------------------------------------
-- | Monad for an n-armed bandit environment
-- AgentT o m a -> (a -> m o) -> m a
-- TODO split states up
runBandit :: Float -> Outcome -> Environment Action
runBandit eps (O.Outcome i r)= do
  --modify $ addStats i (Stats 1 r 0)
  s <- get
  let rwds = Map.map mean s
  a <- epsilonGreedy (Map.toList rwds) eps
  return a

act :: Environment ()
act = replicateM_ 100 $ void $ bandit 0.1

bandit :: Float -> Environment Outcome 
bandit eps = do
  s <- get
  let rwds = Map.map mean s
  a <- epsilonGreedy (Map.toList rwds) eps
  stepEnv a

epsilonGreedy
  :: (R.Variate e, Ord e, Ord r) 
  => [(a, r)] -> e -> Environment a
epsilonGreedy acts = 
  epsilonChoice (fst $ maximumBy (comparing snd) acts) acts

epsilonChoice 
  :: (R.Variate e, Ord e) 
  => a -> [(a, r)] -> e -> Environment a
epsilonChoice a acts eps = do
  g <- reader gen
  compare eps <$> R.uniform g >>= \case
    LT -> pure a
    _  -> do
      i <- R.uniformR (0, length acts) g
      pure . fst . head $ drop (i-1) acts


------------------------------------------------------------------------------
-- | Statistics observed for a particular candidate.
data Stats = Stats
    { armCount :: !Int  -- ^ Number of times this candidate was observed
    , armTotal :: !Double -- ^ Total reward over all observations
    } deriving (Show, Eq)

instance Semigroup Stats where
    s <> s' = Stats { armCount = armCount s + armCount s'
                    , armTotal = armTotal s + armTotal s' }

instance Monoid Stats where
    mempty = Stats 0 0

-- | A record of statistics for all possibilities
type BanditState = Map.Map Action Stats

-- | Average reward over all observations for that arm.
mean :: Stats -> Reward
mean (Stats 0 _) = 0
mean ss = armTotal ss / toEnum (armCount ss)

addStats :: Action -> Stats -> BanditState -> BanditState
addStats = Map.insertWith (<>)


