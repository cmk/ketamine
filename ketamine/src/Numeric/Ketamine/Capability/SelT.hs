{-# LANGUAGE DeriveFunctor #-}

-- Selection monad transformer library
-- A generic backtracking search and auto-pruning library

module SelT (SelT(..), Sel, toCont,
             boundedBinarySearch, unboundedBinarySearch) where

import Control.Monad.Cont
import Data.Functor.Identity

-- Selection monad transformer

newtype SelT r m x = SelT {runSelT :: (x -> m r) -> m x} deriving Functor

instance Monad m => Applicative (SelT r m) where

    pure = return

    (<*>) = ap


instance (Monad m) => Monad (SelT r m) where
         return = SelT . const . return
         e >>= f = SelT $ \p -> let g x = runSelT (f x) p
                                    h x = g x >>= p
                                 in runSelT e h >>= g

instance (MonadIO m) => MonadIO (SelT r m) where
         liftIO = lift . liftIO

instance MonadTrans (SelT r) where
         lift = SelT . const

-- Monad morphism from selections to continuations

toCont :: (Monad m) => SelT r m x -> ContT r m x
toCont e = ContT $ \p -> runSelT e p >>= p

-- Vanilla selection monad

type Sel r = SelT r Identity

-- Generic search functions

unboundedBinarySearch :: (Monad m) => SelT Bool m [Bool]
unboundedBinarySearch = sequence $ repeat $ SelT ($ True)

boundedBinarySearch :: (Monad m) => Int -> SelT Bool m [Bool]
boundedBinarySearch n = sequence $ replicate n $ SelT ($ True)
