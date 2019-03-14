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
import Control.Monad.Loops
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.RWS.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.RWS (RWST, evalRWST)
import Data.List (maximumBy, sort)
import Data.Ord (comparing)
import Data.Vector ((!), Vector)
import Data.IORef

import Numeric.Ketamine.Agent
import Numeric.Ketamine.Environment 
import Numeric.Ketamine.Episode
import qualified Numeric.Ketamine.Outcome as O

import qualified Control.Monad.Trans.Reader as TR
import qualified Data.DList as D
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Statistics.Distribution as Dist
import qualified Statistics.Distribution.Normal as N
import qualified System.Random.MWC as R


-- | The slot machine index whose arm will be pulled
type Action = Int
type Reward = Double
type Outcome = O.Outcome Action Reward


-- TODO: separate transformer stacks and log in Episode
type SharedState = RWST (IORef BanditState) (D.DList Outcome) EnvState IO
type Environment = EnT Action Outcome SharedState
type Agent = AgT Action Outcome SharedState

-------------------------------------------------------------------------------
-- | Episode

episodeLength :: Int
episodeLength = 100

narms :: Int
narms = 10

main :: IO ()
main = do
    casino <- envState narms
    bandit <- defaultBanditState narms
    let ep :: SharedState ()
        ep = runEpisode $ stepEnv >~> stepBandit 0.1 $ 9
    res <- snd <$> evalRWST ep bandit casino
    mapM_ print $ D.toList res

-------------------------------------------------------------------------------
-- | Environment
--
data EnvState = 
    EnvState { arms :: Vector N.NormalDistribution , gen :: R.GenIO, pulls :: Int }

instance Show EnvState where
    show c = "EnvState" ++
      "{ means = " ++ 
        show (fmap Dist.mean . V.toList $ arms c) ++ 
          ", pulls = " ++ show (pulls c) ++ " }"

mkEnvState :: Int -> [Double] -> IO EnvState
mkEnvState n vars = do
    gen <- R.createSystemRandom
    means <- replicateM n $ R.uniform gen
    let arms = V.fromList $ zipWith N.normalDistr (reverse $ sort means) vars
    return $ EnvState arms gen 0

-- | Default config of a n-armed bandit
envState :: Int -> IO EnvState
envState n = mkEnvState n $ replicate n 1.0

genContVar :: Dist.ContGen d => d -> Environment Double
genContVar d = do
    g <- gets gen
    liftIO $ Dist.genContVar d g

stepEnv :: Action -> Environment ()
stepEnv = loop where
    loop act = do
        rwd <- genContVar =<< (! act) . arms <$> get
        modify $ \(EnvState a g p) -> EnvState a g (p+1)
        r <- ask
        liftIO $ modifyIORef r $ addStats act (Stats act rwd)
        n <- gets pulls
        if n >= episodeLength 
            then return () 
            else outcome (O.Outcome act rwd) >>= loop

------------------------------------------------------------------------------
-- | Bandit

 
-- Statistics observed for a particular candidate.
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

epsilonGreedy :: (R.Variate e, Ord e, Ord r) => [(a, r)] -> e -> SharedState a
epsilonGreedy acts = 
    epsilonChoice (fst $ maximumBy (comparing snd) acts) acts

epsilonChoice :: (R.Variate e, Ord e) => a -> [(a, r)] -> e -> SharedState a
epsilonChoice a acts eps = do
    g <- gets gen
    compare eps <$> R.uniform g >>= \case
        LT -> pure a
        _  -> do
            i <- R.uniformR (0, length acts) g
            pure . fst . head $ drop (i-1) acts

banditState :: Int -> Stats -> IO (IORef BanditState)
banditState n s = do
    r <- newIORef mempty
    let init = Map.fromList $ take n $ zip [0..] (repeat s)
    writeIORef r init
    return r

defaultBanditState :: Int -> IO (IORef BanditState)
defaultBanditState n = banditState n mempty

stepBandit :: Float -> Outcome -> Agent ()
stepBandit eps = loop where
    loop o@(O.Outcome act rwd) = do
        tell . pure $ o
        r <- ask
        liftIO $ modifyIORef r $ addStats act (Stats act rwd)
        s <- liftIO $ readIORef r
        let rwds = Map.map mean s
        a <- lift $ epsilonGreedy (Map.toList rwds) eps
        action a >>= loop
