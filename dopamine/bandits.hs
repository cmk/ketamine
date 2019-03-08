{-# LANGUAGE DeriveGeneric, 
             MultiParamTypeClasses, 
             FlexibleInstances, 
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

import qualified Control.Monad.Trans.Class as Trans
import qualified Data.DList as D
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Statistics.Distribution as Dist
import qualified Statistics.Distribution.Normal as N
import qualified System.Random.MWC as R

-- | Default config of a n-armed bandit
defaultConfig :: Int -> IO Config
defaultConfig n = mkConfig n $ replicate n 1.0

act :: Environment ()
act = replicateM_ 100 $ void $ banditStep 0.1

main :: IO ()
main = do
  config <- defaultConfig 10
  res <- runEnvironment config act
  mapM_ print $ D.toList res

------------------------------------------------------------------------------

data Config = Config { arms :: Vector N.NormalDistribution , gen :: R.GenIO }

instance Show Config where
  show c = "Config" ++
    "{ bandit_means = " ++ show (fmap Dist.mean . V.toList $ arms c) ++ " }"

mkConfig :: Int -> [Double] -> IO Config
mkConfig n vars = do
  gen <- R.createSystemRandom
  means <- replicateM n $ R.uniform gen
  let arms = V.fromList $ zipWith N.normalDistr (reverse $ sort means) vars
  return $ Config arms gen

epsilonGreedy
  :: (R.Variate e, Ord e, Ord r) 
  => [(a, r)] -> e -> Environment a
epsilonGreedy acts = epsilonChoice (fst $ maximumBy (comparing snd) acts) acts

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

banditStep :: Float -> Environment (Obs Rew StatsMap)
banditStep eps = do
  s <- get
  let rwds = Map.map mean s
  a <- epsilonGreedy (Map.toList rwds) eps
  step a

------------------------------------------------------------------------------
-- | Statistics observed for a particular candidate.
data Stats = Stats
    { count       :: !Int  -- ^ Number of times this candidate was observed
    , totalRew :: !Rew  -- ^ Total reward over all observations
    } deriving (Show, Eq)

instance Semigroup Stats where
    s <> s' = Stats { count = count s + count s'
                    , totalRew = totalRew s + totalRew s' }

instance Monoid Stats where
    mempty = Stats 0 0

-- | A record of statistics for all possibilities
type StatsMap = Map.Map Act Stats

-- | The slot machine index whose arm will be pulled
type Act = Int
type Rew = Double
type Event = (Act, Rew) 

-- | Average reward over all observations.
mean :: Stats -> Rew
mean (Stats 0 _) = 0
mean ss = totalRew ss / toEnum (count ss)


addStats :: Act -> Stats -> StatsMap -> StatsMap
addStats = Map.insertWith (<>)

------------------------------------------------------------------------------
-- | Monad for an n-armed bandit environment
newtype Environment a = 
  Environment { getEnvironment :: RWST Config (D.DList Event) StatsMap IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader Config
    , MonadWriter (D.DList Event)
    , MonadState StatsMap
    , MonadRWS Config (D.DList Event) StatsMap
    )

instance PrimMonad Environment where
  type PrimState Environment = RealWorld
  primitive = Environment . primitive

data Initial o = Initial !o | EmptyEpisode

data Obs r o = Next !r !o | Done !r !(Maybe o) | Terminated
  deriving (Show, Eq)

--instance MonadEnv Environment StatsMap Act Rew where
reset :: Environment (Initial StatsMap)
reset = do
  n <- reader $ V.length . arms
  let init = Map.fromList $ take n $ zip [0..] (repeat mempty)
  put init
  return $ Initial init

step :: Act -> Environment (Obs Rew StatsMap)
step action = do
  rwd <- genContVar =<< (! action) . arms <$> ask
  modify $ addStats action (Stats 1 rwd)
  s <- get
  tell . pure $ (action, rwd)
  return $ Next rwd s

genContVar :: Dist.ContGen d => d -> Environment Double
genContVar d = do
  g <- reader gen
  liftIO $ Dist.genContVar d g

-- | Run an n-armed bandit environment
runEnvironment :: Config -> Environment a -> IO (D.DList Event)
runEnvironment c (Environment m) = snd <$> evalRWST m' c Map.empty
  where (Environment i) = reset
        m' = i >> m
