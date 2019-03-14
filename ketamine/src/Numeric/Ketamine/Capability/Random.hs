{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Numeric.Ketamine.Capability.Random
    (
    -- * MonadRandom
      MonadRandom                       (..)
    , getRandom

    -- * Random
    , Random
    , runRandom
    , evalRandom

    -- * RandomT
    , RandomT
    , runRandomT
    , evalRandomT

    -- ** Sources
    -- *** StdGen
    , Std.StdGen
    , Std.mkStdGen
    , Std.newStdGen

    -- *** \/dev\/random
    , Dev.DevRandom                     (..)

    -- *** MWC256
    , MWC.Gen
    , MWC.create
    , MWC.initialize
    , MWC.save
    , MWC.restore

    -- *** Mersenne Twister
    , Pure.PureMT
    , Pure.newPureMT
    , Pure.pureMT

    -- *** Threefish
    , TF.TFGen
    , TF.seedTFGen
    , TF.newTFGen

    -- *** Murmur64
    , Murmur.Hash64
    , Murmur.Hashable64(..)
    , Murmur.asWord64
    , Murmur.combine
    , Murmur.hash64
    , Murmur.hash64AddWord64
    , runSeededRandom

    -- ** Distributions
    -- *** Uniform
    , Distribution.StdUniform           (..)
    , Distribution.Uniform              (..)
    , DiscreteUniform
    , mkDiscreteUniform, mkDiscreteUniform'
    -- *** Normal
    , Distribution.Normal               (..)
    -- *** Categorical
    , Distribution.fromWeightedList
    -- *** Other Distributions
    , Distribution.Beta                 (..)
    , Distribution.Binomial             (..)
    , Distribution.Categorical
    , Distribution.ChiSquare            (..)
    , Distribution.Dirichlet            (..)
    , Distribution.Exponential          (..)
    , Distribution.Gamma                (..)
    , Distribution.Multinomial          (..)
    , Distribution.Pareto               (..)
    , Distribution.Poisson              (..)
    , Distribution.Rayleigh             (..)
    , Distribution.StdSimplex           (..)
    , Distribution.StretchedExponential (..)
    , Distribution.T                    (..)
    , Distribution.Triangular           (..)
    , Distribution.Weibull              (..)
    , Distribution.Ziggurat             (..)

    -- *** Distribution Class
    , Distribution                      (..)

    -- ** RVar
    , RVar
    , RVarT

    -- ** Helpers
    , categorize
    , priorProb
    , sampleSeeded
    ) where

import Control.Applicative          (Alternative)
import Control.Monad                (MonadPlus)
import Control.Monad.Fail           (MonadFail)
import Control.Monad.Fix            (MonadFix)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Primitive      (PrimMonad, PrimState, RealWorld,
                                     primToPrim)
import Control.Monad.ST             (ST)
import Control.Monad.Trans.Class    (MonadTrans (lift))
import Control.Monad.Trans.Lift.StT (StT)

import Data.Functor.Identity        (Identity, runIdentity)
import Data.List                    (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Random.Distribution     (Distribution)
import Data.RVar                    (RVar, RVarT)

import Numeric.Ketamine.Capability.State           (StateT)

import qualified Data.Digest.Murmur64 as Murmur

import qualified Data.Random                  as Random
import qualified Data.Random.Source           as Source
import qualified Data.Random.Source.DevRandom as Dev
import qualified Data.Random.Source.MWC       as MWC
import qualified Data.Random.Source.PureMT    as Pure
import qualified Data.Random.Source.StdGen    as Std
import qualified System.Random.TF             as TF
import qualified System.Random.TF.Instances   as TF
import qualified Numeric.Ketamine.Capability.Reader           as Reader
import qualified Numeric.Ketamine.Capability.State            as State

import qualified Data.Random.Distribution.Beta                 as Distribution
import qualified Data.Random.Distribution.Binomial             as Distribution
import qualified Data.Random.Distribution.Categorical          as Distribution
import qualified Data.Random.Distribution.ChiSquare            as Distribution
import qualified Data.Random.Distribution.Dirichlet            as Distribution
import qualified Data.Random.Distribution.Exponential          as Distribution
import qualified Data.Random.Distribution.Gamma                as Distribution
import qualified Data.Random.Distribution.Multinomial          as Distribution
import qualified Data.Random.Distribution.Normal               as Distribution
import qualified Data.Random.Distribution.Pareto               as Distribution
import qualified Data.Random.Distribution.Poisson              as Distribution
import qualified Data.Random.Distribution.Rayleigh             as Distribution
import qualified Data.Random.Distribution.Simplex              as Distribution
import qualified Data.Random.Distribution.StretchedExponential as Distribution
import qualified Data.Random.Distribution.T                    as Distribution
import qualified Data.Random.Distribution.Triangular           as Distribution
import qualified Data.Random.Distribution.Uniform              as Distribution
import qualified Data.Random.Distribution.Weibull              as Distribution
import qualified Data.Random.Distribution.Ziggurat             as Distribution

-- FIXME: document and re-export Distributions, common types, etc.

class Monad m => MonadRandom m where
    liftRVar :: RVar a -> m a

instance MonadRandom RVar where
    liftRVar = id
    {-# INLINE liftRVar #-}

instance {-# OVERLAPPABLE #-}
    ( MonadTrans t
    , Monad (t m)
    , MonadRandom m
    ) => MonadRandom (t m)
  where
    liftRVar = lift . liftRVar
    {-# INLINE liftRVar #-}


-- TODO: add notes about composition of variates, summing, etc. and
-- the purpose of 'RVar'.
getRandom :: (MonadRandom m, Distribution d a) => d a -> m a
getRandom = liftRVar . Random.rvar
{-# INLINE getRandom #-}

-- | The random monad parameterized by the underlying generator, @g@.
type Random g = RandomT g Identity

-- | Run the monadic computation, returning the computed value and the
-- generator state.
runRandom :: g -> Random g a -> (a, g)
runRandom g = runIdentity . runRandomT g
{-# INLINE runRandom #-}

-- | Run the monadic computation, returning the computed value and discarding
-- the generator state.
evalRandom :: g -> Random g a -> a
evalRandom g = runIdentity . evalRandomT g
{-# INLINE evalRandom #-}

-- | The random transformer monad parameterized by:
--
-- * @g@ - The generator.
--
-- * @m@ - The inner monad.
--
newtype RandomT g m a = RandomT { unRandomT :: StateT g m a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadTrans
        , MonadFix
        , MonadFail
        , MonadPlus
        , MonadIO
        , Reader.LiftLocal
        )

-- | Run the monadic computation, returning the computed value and the
-- generator state.
runRandomT :: g -> RandomT g m a -> m (a, g)
runRandomT g m = State.runStateT (unRandomT m) g
{-# INLINE runRandomT #-}

-- | Run the monadic computation, returning the computed value and discarding
-- the generator state.
evalRandomT :: Monad m => g -> RandomT g m a -> m a
evalRandomT g m = State.evalStateT (unRandomT m) g
{-# INLINE evalRandomT #-}


type instance StT (RandomT g) a = StT (State.StateT g) a

instance Monad m => MonadRandom (RandomT Std.StdGen m) where
    liftRVar = RandomT . Random.sample
    {-# INLINE liftRVar #-}

instance Monad m => MonadRandom (RandomT Pure.PureMT m) where
    liftRVar = RandomT . Random.sample
    {-# INLINE liftRVar #-}

instance MonadIO m => MonadRandom (RandomT Dev.DevRandom m) where
    liftRVar v = RandomT $
        State.get @Dev.DevRandom
            >>= liftIO . Random.runRVar v
    {-# INLINE liftRVar #-}

instance {-# OVERLAPPABLE #-}
         ( PrimMonad m
         , PrimState m ~ s
         ) => MonadRandom (RandomT (MWC.Gen s) m) where
    liftRVar v = RandomT $
        let f = primToPrim :: ST s a -> m a
         in State.get @(MWC.Gen s)
            >>= lift . f . flip Random.sampleFrom v
    {-# INLINE liftRVar #-}

instance ( PrimMonad m
         , PrimState m ~ RealWorld
         ) => MonadRandom (RandomT (MWC.Gen RealWorld) m) where
    liftRVar v = RandomT $
        let f = primToPrim :: IO a -> m a
         in State.get @(MWC.Gen RealWorld)
            >>= lift . f . flip Random.sampleFrom v
    {-# INLINE liftRVar #-}

tfRandom :: (Monad m, TF.Random a) => RandomT TF.TFGen m a
tfRandom = RandomT $ do
    g <- State.get @TF.TFGen
    let (x, g') = TF.random g
    State.put $! g'
    return x
{-# INLINE tfRandom #-}

-- Generate a random-fu MonadRandom instance for tf-random.
-- Note that `getRandomDouble` will be generated automatically from
-- `getRandomWord64` by the TemplateHaskell.  (tf-random doesn't provide
-- a concrete instance itself.)
Source.monadRandom [d|
    instance Monad m => Source.MonadRandom (RandomT TF.TFGen m) where
        getRandomWord8 = tfRandom
        getRandomWord16 = tfRandom
        getRandomWord32 = tfRandom
        getRandomWord64 = tfRandom
    |]

instance Monad m => MonadRandom (RandomT TF.TFGen m) where
    liftRVar = Random.sample

-- * Discrete uniform distribution

-- | A distribution that chooses from a bounded set of values with equal
-- probability. Construct values of this type using 'mkDiscreteUniform'
-- and 'mkDiscreteUniform''
data DiscreteUniform a r where
  DiscreteUniform :: !Int
                  -> NonEmpty.NonEmpty a
                  -> DiscreteUniform a a
deriving instance Show a => Show (DiscreteUniform a r)

-- | Smart constructor for 'DiscreteUniform' to create a distribution that
-- chooses one of the given choices with equal probability. Returns 'Nothing'
-- if the supplied list is empty.
mkDiscreteUniform :: [a] -> Maybe (DiscreteUniform a a)
mkDiscreteUniform = fmap mkDiscreteUniform' . NonEmpty.nonEmpty

-- | Smart constructor for 'DiscreteUniform' to createa distribution that
-- chooses one of the given choices with equal probability.
mkDiscreteUniform' :: NonEmpty.NonEmpty a -> DiscreteUniform a a
mkDiscreteUniform' xs = DiscreteUniform (NonEmpty.length xs) xs

instance Distribution (DiscreteUniform a) a where
  rvar (DiscreteUniform len xs) =
    fmap (xs NonEmpty.!!) (Distribution.uniform 0 (len - 1))

-- | Run a random effect, seeded from the given hash.  Allows us to run
--
-- The `Hash64` seed can be constructed from an input source using 'hash64'.
runSeededRandom :: Monad m => Murmur.Hash64 -> RandomT TF.TFGen m a -> m a
runSeededRandom h = evalRandomT $ TF.seedTFGen (0, 0, 0, Murmur.asWord64 h)

categorize :: Num p => DiscreteUniform a b -> Distribution.Categorical p a
categorize (DiscreteUniform _ xs) =
  Distribution.fromList . zip (repeat 1) $ NonEmpty.toList xs

priorProb :: (Num p, Eq a) => Distribution.Categorical p a -> a -> Maybe p
priorProb d c = fmap fst . find (match c) $ Distribution.toList d
  where match y (_,y') = y == y'

sampleSeeded :: (Monad m, Distribution d t) => Murmur.Hash64 -> d t -> m t
sampleSeeded s = runSeededRandom s . liftRVar . Random.rvar
