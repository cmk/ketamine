module Numeric.Ketamine.Environment.State where

import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Trans.Reader

{-
newtype StateST s a = StateST 
    { runStateST :: forall r. ReaderT (STRef r s) (ST r) a }

runState :: StateST s a -> s -> (a,s)
runState m s0 = runST (do
    r <- newSTRef s0
    a <- runReaderT (runStateST m) r
    s <- readSTRef r
    return (a,s))

instance Monad (StateST s) where
    return a = StateST (return a)
    m >>= f  = StateST (runStateST m >>= runStateST . f)

instance MonadState s (StateST s) where
    get   = State (ask >>= lift . readSTRef)
    put x = State (ask >>= \s -> lift (writeSTRef s x))

newtype StateST s m a = StateST { unStateST :: ReaderT s m a } 
  deriving (Functor, Applicative, Monad)



instance MonadTrans (StateST s) where

  lift = StateST . Trans.lift
      

-}


