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
import Control.Monad.RWS.Class
import Control.Monad.IO.Class
import Control.Monad.Writer.Class
import Control.Monad.Primitive
import Control.Monad.State.Class
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS (RWST, evalRWST)
import Data.Functor.Identity
import Data.List (maximumBy, sort)
import Data.Ord (comparing)
import Data.Vector ((!), Vector)

import Numeric.Dopamine.Environment
import qualified Numeric.Dopamine.Outcome as O

import qualified Control.Monad.Trans.Class as Trans
import qualified Data.DList as D
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Statistics.Distribution as Dist
import qualified Statistics.Distribution.Normal as N
import qualified System.Random.MWC as R

newtype EnvState a = 
  EnvState { getEnvState :: RWST Config (D.DList Outcome) StatsMap IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader Config
    , MonadWriter (D.DList Outcome)
    , MonadState StatsMap
    , MonadRWS Config (D.DList Outcome) StatsMap
    )

instance PrimMonad EnvState where
  type PrimState EnvState = RealWorld
  primitive = EnvState . primitive

type Outcome = O.Outcome Action Double

-- | The slot machine index whose arm will be pulled
type Action = Int
type Reward = Double
type BanditEnv = EnvT Outcome EnvState

-- | Default config of a n-armed bandit
defaultConfig :: Int -> IO Config
defaultConfig n = mkConfig n $ replicate n 1.0

episodeLength = 30


main :: IO ()
main = do
  config <- defaultConfig 10
  res <- runEnvState config act
  mapM_ print $ D.toList res
  print config

askEnv :: MonadEnv Config Config m e => e m Config
askEnv = view' $ \s -> pulls s <= episodeLength

--banditView :: BanditEnv s
--banditView = do { s <- get; view' (const True) }

--step steps :: MonadEnv s Outcome EnvState e => e EnvState Action -> EnvState Outcome

-- | Run an n-armed bandit environment
runEnvState :: Config -> EnvState a -> IO (D.DList Outcome)
runEnvState c (EnvState m) = snd <$> evalRWST (i >> m) c Map.empty
  where (EnvState i) = reset

------------------------------------------------------------------------------

data Config = 
  Config { arms :: Vector N.NormalDistribution , gen :: R.GenIO, pulls :: Integer }

instance Show Config where
  show c = "Config" ++
    "{ means = " ++ show (fmap Dist.mean . V.toList $ arms c) ++ ", pulls = " ++ show (pulls c) ++ " }"

mkConfig :: Int -> [Double] -> IO Config
mkConfig n vars = do
  gen <- R.createSystemRandom
  means <- replicateM n $ R.uniform gen
  let arms = V.fromList $ zipWith N.normalDistr (reverse $ sort means) vars
  return $ Config arms gen 0

reset :: EnvState StatsMap
reset = do
  n <- reader $ V.length . arms
  let init = Map.fromList $ take n $ zip [0..] (repeat mempty)
  put init
  return init

steps :: Action -> EnvState Outcome
steps action = do
  rwd <- genContVar =<< (! action) . arms <$> ask
  modify $ addStats action (Stats action rwd)
  s <- get
  tell . pure $ O.Outcome action rwd
  return $ O.Outcome action rwd

genContVar :: Dist.ContGen d => d -> EnvState Double
genContVar d = do
  g <- reader gen
  liftIO $ Dist.genContVar d g

------------------------------------------------------------------------------
-- | Monad for an n-armed bandit environment

-- TODO split states up
runBandit :: Float -> Outcome -> EnvState Action
runBandit eps (O.Outcome i r)= do
  --modify $ addStats i (Stats 1 r 0)
  s <- get
  let rwds = Map.map mean s
  a <- epsilonGreedy (Map.toList rwds) eps
  return a

act :: EnvState ()
act = replicateM_ 100 $ void $ bandit 0.1

bandit :: Float -> EnvState Outcome 
bandit eps = do
  s <- get
  let rwds = Map.map mean s
  a <- epsilonGreedy (Map.toList rwds) eps
  steps a

epsilonGreedy
  :: (R.Variate e, Ord e, Ord r) 
  => [(a, r)] -> e -> EnvState a
epsilonGreedy acts = 
  epsilonChoice (fst $ maximumBy (comparing snd) acts) acts

epsilonChoice 
  :: (R.Variate e, Ord e) 
  => a -> [(a, r)] -> e -> EnvState a
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
type StatsMap = Map.Map Action Stats

-- | Average reward over all observations for that arm.
mean :: Stats -> Reward
mean (Stats 0 _) = 0
mean ss = armTotal ss / toEnum (armCount ss)

addStats :: Action -> Stats -> StatsMap -> StatsMap
addStats = Map.insertWith (<>)


