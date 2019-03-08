{-# LANGUAGE AllowAmbiguousTypes,
             ConstraintKinds, 
             MultiParamTypeClasses, 
             FlexibleContexts, 
             FlexibleInstances, 
             FunctionalDependencies, 
             GeneralizedNewtypeDeriving,
             UndecidableInstances, 
             TypeFamilies, 
             DeriveFunctor, 
             DeriveGeneric,
             ScopedTypeVariables, 
             TypeApplications
#-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Numeric.Dopamine.Environment (
  module Numeric.Dopamine.Environment.Class
) where


import Control.Applicative (Alternative(..),liftA2)
import Control.Exception.Safe 
import Control.Monad
import Control.Monad.Cont.Class 
import Control.Monad.IO.Class
import Control.Monad.Morph (MFunctor(..), MMonad(..), generalize)
import Control.Monad.Primitive
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.ST
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Control.Monad.Trans.Identity (IdentityT(..), mapIdentityT)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bool (bool)
import Data.Default
import Data.Functor.Identity
import Data.List (maximumBy, sort)
import Data.Maybe
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Vector ((!), Vector)
import Data.Word

import Numeric.Dopamine.Environment.Class

import qualified Control.Monad.Trans.Class as Trans

data Outcome o r = Outcome { observation :: o, reward :: r }


shift :: MonadEnv s o m e => ((s -> m o) -> e m o) -> e m s
shift = undefined

shift' :: (MonadEnv s a Maybe e, MonadThrow m) => ((s -> m a) -> e Maybe a) -> e Maybe s
shift' f = shift $ f . (maybe (throwM SessionOver) return .)

shift''
  :: (MonadEnv s o m1 e, MonadThrow m2) =>
     (((a1 -> Maybe a2) -> a1 -> m2 a2) -> (s -> m1 o) -> e m1 o)
     -> e m1 s
shift'' f = shift . f $ (maybe (throwM SessionOver) return .) -- $ \g -> (foo . g)

bar :: MonadEnv s o Maybe e => e Maybe s
bar = shift' $ viewEnv


withAction :: (a -> StateT s Maybe o) -> a -> s -> Maybe o
withAction f a s = fmap fst . (`runStateT` s) $ f a

-- | Lift a pure state transition into an environment.
lift :: MonadEnv s o m e => (a -> StateT s Maybe o) -> a -> e m o
lift f a = view $ withAction f a

lift' :: MonadEnv s s m e => (a -> StateT s Maybe o) -> a -> e m s
lift' f a = view' . (isJust .) $ withAction f a

lift'' :: MonadEnv (Maybe s) o m e => (a -> StateT Bool Maybe o) -> a -> e m o
lift'' f a = view . (. isJust) $ withAction f a

lower :: MonadEnv s o m e => e m o -> m o
lower = lowerEnv

over :: MonadEnv s o m e => (m o -> m o) -> e m a -> e m a
over f = withEnv (f .)

-- | Use a state transition to advance environment one step.
step :: MonadEnv s o m e => (a -> m o) -> e m a -> m o
step f = lower . withEnv (const f)

-- | Use a pure state transition to advance environment one step.
step' :: forall a s o m e. MonadEnv s o m e => (a -> StateT s Maybe o) -> e m a -> m o
step' f = step $ lower . lift @_ @_ @_ @e f

step'' :: (MonadEnv s o m e, MonadThrow m, Default s) => (a -> StateT s Maybe o) -> e m a -> m o
step'' f = stepThrow $ \a -> withAction f a def

stepThrow :: (MonadEnv s o m e, MonadThrow m) => (a -> Maybe o) -> e m a -> m o
stepThrow f = step $ maybe (throwM SessionOver) return . f

view :: MonadEnv s o m e => (s -> Maybe o) -> e m o
view = viewEnv

view' :: MonadEnv s s m e => (s -> Bool) -> e m s
view' f = view $ \s -> bool Nothing (Just s) (f s)

views :: forall s o m e. MonadEnv s o m e => (s -> Maybe o) -> m o
views = lower @_ @_ @_ @e . view

views' :: forall s m e. MonadEnv s s m e => (s -> Bool) -> m s
views' = lower @_ @_ @_ @e . view'

viewsThrow :: (MonadEnv s o m e, MonadThrow m) => ((s -> m o) -> e m o) -> (s -> Maybe o) -> m o
viewsThrow f g = lower $ f $ maybe (throwM SessionOver) return . g



reset :: Monad m => EnvT a m a -> EnvT a' m a
reset e = Trans.lift $ runEnvT e return

--resetT :: Monad m => ContT r m r -> ContT r' m r

--shiftT :: Monad m => ((a -> m o) -> EnvT o m o) -> EnvT o m a

--fromEnv :: Functor m => EnvT s a m a -> EnvState s m a
--fromEnv = evalContT . runEnvT 

-- ContT o (MaybeT (StateT s m)) a 
-- (a -> StateT s Maybe o) -> StateT s Maybe o
