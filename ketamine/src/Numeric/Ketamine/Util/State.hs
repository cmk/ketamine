{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor, RankNTypes #-}
{-# LANGUAGE TemplateHaskell            #-}

 {-# OPTIONS_GHC -w #-}
-- | State effect and handlers.
module Numeric.Ketamine.Util.State where

import Control.Monad (ap)
import Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Primitive      (PrimMonad, PrimState, RealWorld)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class    (MonadState(..))
import           Control.Monad.Primitive  (PrimMonad (..))
import qualified Data.Random.Source           as Source
import Control.Monad.ST
import Control.Monad.Primitive

import Control.Monad.Trans.Reader (ReaderT(..))
import Data.STRef

import Numeric.Ketamine.Util.Random

newtype StateST s a = StateST  { unStateST :: forall r. ReaderT (STRef r s) (ST r) a }
  deriving Functor

runStateST :: StateST s a -> s -> (a,s)
runStateST st s0 = runST $ do
    r <- newSTRef s0
    a <- runReaderT (unStateST st) r
    s <- readSTRef r
    return (a,s)

evalStateST :: StateST s a -> s -> a
evalStateST st s0 = fst $ runStateST st s0


instance Applicative (StateST s) where
    pure = return
    (<*>) = ap

instance Monad (StateST s) where
    return a = StateST (return a)
    m >>= f  = StateST (unStateST m >>= unStateST . f)

instance MonadState s (StateST s) where
    get   = StateST $ ask >>= lift . readSTRef
    put x = StateST $ ask >>= \s -> lift (writeSTRef s x)

{-
instance MonadReader s (StateST s) where
    ask   = StateST $ ask >>= lift . readSTRef
    local f s = undefined -- StateST $ ask >>= lift . readSTRef
-}

-- Generate a random-fu MonadRandom instance for tf-random.
-- Note that `getRandomDouble` will be generated automatically from
-- `getRandomWord64` by the TemplateHaskell.  (tf-random doesn't provide
-- a concrete instance itself.)
Source.monadRandom [d|
    instance Source.MonadRandom (StateST TFGen) where
        getRandomWord8 = tfRandom
        getRandomWord16 = tfRandom
        getRandomWord32 = tfRandom
        getRandomWord64 = tfRandom
    |]

--runSeededRandom :: Monad m => Murmur.Hash64 -> RandomT TF.TFGen m a -> m a
--runSeededRandom h = evalRandomT $ TF.seedTFGen (0, 0, 0, Murmur.asWord64 h)

runSeededST :: Hash64 -> StateST TFGen a -> a
runSeededST h st = evalStateST st $ runSeeded h


