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

module Numeric.Ketamine.Effect.Random
    (
    -- * HasRandom
      HasRandom                       (..)
    --, getRandom



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
    , Distribution                      (..)

    -- ** RVar
    , RVar
    , RVarT

    -- ** Helpers
    , categorize
    , priorProb
    --, sampleSeeded
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

--import Control.Monad.Trans.State           (StateT)


import qualified Control.Monad.Trans.Reader  as Reader
import qualified Control.Monad.State.Class    as State
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

class HasRandom a env | env -> a where
    random :: env -> RVar a


instance HasRandom a (RVar a) where
    random = id
    {-# INLINE random #-}

    
{-
-- TODO: add notes about composition of variates, summing, etc. and
-- the purpose of 'RVar'.
getRandom :: (HasRandom m, Distribution d a) => d a -> m a
getRandom = random . Random.rvar
{-# INLINE getRandom #-}
-}


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


