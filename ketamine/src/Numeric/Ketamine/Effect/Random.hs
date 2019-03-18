{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
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

 {-# OPTIONS_GHC -w #-}
module Numeric.Ketamine.Effect.Random
    (
    -- * HasRVar
      HasRVar                       (..)
    , getRandom
    , tfRandom


    -- ** Sources
    -- *** StdGen
    , Std.StdGen
    , Std.mkStdGen
    , Std.newStdGen

    -- *** \/dev\/rvar
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
    --, runSeededRandom

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
    --, Distribution                      (..)

    -- ** RVar
    , RVar
    , RVarT

    -- ** Helpers
    , categorize
    , priorProb
    , runSeeded
    --, sampleSeeded
    ) where

import Control.Applicative          (Alternative)
import Control.Monad                (MonadPlus)
import Control.Monad.Fail           (MonadFail)
import Control.Monad.Fix            (MonadFix)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Primitive      (PrimMonad, PrimState, RealWorld,
                                     primToPrim)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)

import Control.Monad.ST             (ST)
import Control.Monad.Trans.Class    (MonadTrans (lift))

import Data.Functor.Identity        (Identity, runIdentity)
import Data.List                    (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Random.Distribution     (Distribution)
import Data.RVar                    (RVar, RVarT)

import System.Random.TF.Gen (RandomGen(..))
import System.Random.TF.Instances (Random)

import Numeric.Ketamine.Types

import qualified Control.Monad.Trans.Reader  as Reader
import qualified Control.Monad.State   as State
--import qualified Control.Monad.Trans.State    as State

import qualified Data.Digest.Murmur64 as Murmur

import qualified Data.Random                  as Random
import qualified Data.Random.Source           as Source
import qualified Data.Random.Source.DevRandom as Dev
import qualified Data.Random.Source.MWC       as MWC
import qualified Data.Random.Source.PureMT    as Pure
import qualified Data.Random.Source.StdGen    as Std
import qualified System.Random.TF             as TF
import qualified System.Random.TF.Instances   as TF
import qualified System.Random.MWC as MWC


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

class HasGen s e | e -> s where
    gen :: Lens' e (MWC.Gen s)

instance HasGen s (MWC.Gen s) where
    gen = id
    {-# INLINE gen #-}

class HasRVar r e | e -> r where
    rvar :: Lens' e (RVar r)

instance HasRVar r (RVar r) where
    rvar = id
    {-# INLINE rvar #-}

-- TODO: add notes about composition of variates, summing, etc. and
-- the purpose of 'RVar'.
getRandom 
  :: forall (m :: * -> *) r s. ()
  => Distribution (RVarT Identity) r
  => MonadReader s m
  => HasRVar r s
  => m (RVar r)
-- getRandom :: (Env (d r) e, MonadReader e m, Distribution d r) => m r
getRandom = Random.rvar <$> view rvar


tfRandom
  :: (MonadState s m, Random b, RandomGen s) => m b
tfRandom =  do
    g <- State.get
    let (x, g') = TF.random g
    State.put $! g'
    return x
{-# INLINE tfRandom #-}




{-
foo :: forall b s1 s2 (t :: (* -> *) -> * -> *) (m :: * -> *) (d :: * -> *).
               (MonadReader s1 (t m), HasRef s2 s1, MonadTrans t,
                Control.Monad.Primitive.PrimBase m, Distribution d b,
                Source.RandomSource m (SomeRef s2), PrimMonad m) => d b -> t m b       
foo v = view ref >>= lift . primToPrim . flip Random.sampleFrom v

bar :: forall b s1 s2 (t :: (* -> *) -> * -> *) (m :: * -> *) (d :: * -> *).
               (MonadReader s1 (t m), HasRef s2 s1, MonadTrans t,
                Distribution d b, Source.RandomSource m (SomeRef s2)) => d b -> t m b
bar v = view ref >>= lift . flip Random.sampleFrom v
-}

-- | Run a random effect, seeded from the given hash.  Allows us to run
--
-- The `Hash64` seed can be constructed from an input source using 'hash64'.
--runSeededRandom :: HasRef TF.TFGen e => Murmur.Hash64 -> Keta TF.TFGen a -> IO a
runSeeded :: Murmur.Hash64 -> TF.TFGen
runSeeded h = TF.seedTFGen (0, 0, 0, Murmur.asWord64 h)


--runSeededRandom :: Monad m => Murmur.Hash64 -> RandomT TF.TFGen m a -> m a
--runSeededRandom h = evalRandomT $ TF.seedTFGen (0, 0, 0, Murmur.asWord64 h)

--sampleSeeded :: (Monad m, Distribution d t) => Murmur.Hash64 -> d t -> m t
--sampleSeeded s = runSeededRandom s . liftRVar . Random.rvar


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

categorize :: Num p => DiscreteUniform a b -> Distribution.Categorical p a
categorize (DiscreteUniform _ xs) =
  Distribution.fromList . zip (repeat 1) $ NonEmpty.toList xs

priorProb :: (Num p, Eq a) => Distribution.Categorical p a -> a -> Maybe p
priorProb d c = fmap fst . find (match c) $ Distribution.toList d
  where match y (_,y') = y == y'


