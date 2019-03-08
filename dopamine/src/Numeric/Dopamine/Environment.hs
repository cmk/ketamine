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
  module Numeric.Dopamine.Environment.Class,
  module Numeric.Dopamine.Environment
) where


import Control.Exception.Safe 
import Control.Monad
import Control.Monad.Cont.Class 
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Default
import Data.Maybe

import Numeric.Dopamine.Episode
import Numeric.Dopamine.Environment.Class
import Numeric.Dopamine.Exception (EpisodeCompleted(..))

import qualified Control.Monad.Trans.Class as Trans


lift :: MonadEnv s o m e => m o -> e m o
lift = Trans.lift

lower :: MonadEnv s o m e => e m o -> m o
lower = lowerEnv

over :: MonadEnv s o m e => (m o -> m o) -> e m a -> e m a
over f = withEnv (f .)

view :: MonadEnv s o m e => (s -> Maybe o) -> e m o
view = viewEnv

-- A version of 'view' specialized to evironments where the full state is visible to an agent.
view' :: MonadEnv s s m e => (s -> Bool) -> e m s
view' = view . simple

simple :: (t -> Bool) -> t -> Maybe t
simple f s = if f s then Just s else Nothing

-----------------------------------------------------------------------------------------
-- | Lift a pure state transition into an environment.
--
-- The 'State' monad provides a minimal DSL sufficient for describing simple environments.
liftS :: MonadEnv s o m e => (a -> StateT s Maybe o) -> a -> e m o
liftS f a = view $ runStep f a

-- A version of 'liftS' specialized to evironments where the full state is visible to an agent.
liftS' :: MonadEnv s s m e => (a -> StateT s Maybe o) -> a -> e m s
liftS' f a = view' . (isJust .) $ runStep f a

runStep :: (a -> StateT s Maybe o) -> a -> s -> Maybe o
runStep f a s = fmap fst . (`runStateT` s) $ f a

-- | Use a state transition to advance an environment one step.
step :: MonadEnv s o m e => (a -> m o) -> e m a -> m o
step f = lower . withEnv (const f)

-- | Use a pure state transition to advance an environment one step.
stepS :: forall a s o m e. MonadEnv s o m e => (a -> StateT s Maybe o) -> e m a -> m o
stepS f = step $ lower . liftS @_ @_ @_ @e f

stepE :: (MonadEnv s o m e, MonadThrow m) => (a -> Maybe o) -> e m a -> m o
stepE f = step $ maybe (throwM EpisodeCompleted) return . f

stepE' :: (MonadEnv s s m e, MonadThrow m) => (s -> Bool) -> e m s -> m s
stepE' = stepE . simple



{-

step'' :: (MonadEnv s o m e, MonadThrow m, Default s) => (a -> StateT s Maybe o) -> e m a -> m o
step'' f = stepE $ \a -> runStep f a def

reset :: Monad m => EnvT a m a -> EnvT a' m a
reset e = Trans.lift $ runEnvT e return

shift :: MonadEnv s o m e => ((s -> m o) -> e m o) -> e m s
shift = undefined

shift' :: (MonadEnv s a Maybe e, MonadThrow m) => ((s -> m a) -> e Maybe a) -> e Maybe s
shift' f = shift $ f . (maybe (throwM EpisodeOver) return .)

shift''
  :: (MonadEnv s o m e, MonadThrow m) =>
     (((a -> Maybe b) -> a -> m b) -> (s -> m o) -> e m o)
     -> e m s
shift'' f = shift . f $ (maybe (throwM EpisodeOver) return .)

bar :: MonadEnv s o Maybe e => e Maybe s
bar = shift' $ viewEnv

views :: forall s o m e. MonadEnv s o m e => (s -> Maybe o) -> m o
views = lower @_ @_ @_ @e . view

-- A version of 'views' specialized to evironments where the full state is visible to an agent.
views' :: forall s m e. MonadEnv s s m e => (s -> Bool) -> m s
views' = views @_ @_ @_ @e . simple 

viewShift :: (MonadEnv s o m e, MonadThrow m) => ((s -> m o) -> e m o) -> (s -> Maybe o) -> m o
viewShift f g = lower $ f $ maybe (throwM EpisodeCompleted) return . g

viewShift' :: (MonadEnv s o m1 e, MonadThrow m2) =>
                    ((a -> m2 a) -> e m1 o) -> (a -> Bool) -> m1 o
--viewShift' :: (MonadEnv s o m e, MonadThrow m) => ((s -> m o) -> e m o) -> (s -> Maybe o) -> m o
viewShift' f g = lower $ f $ maybe (throwM EpisodeCompleted) return . (\s -> bool Nothing (Just s) (g s))

lift'' :: MonadEnv (Maybe s) o m e => (a -> StateT Bool Maybe o) -> a -> e m o
lift'' f a = view . (. isJust) $ runStep f a
-}
