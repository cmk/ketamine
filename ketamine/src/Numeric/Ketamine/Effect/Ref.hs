{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor, RankNTypes #-}
{-# LANGUAGE TemplateHaskell            #-}

 {-# OPTIONS_GHC -w #-}
-- | State effect and handlers.
module Numeric.Ketamine.Effect.Ref where

import Control.Lens (Lens', lens, view)
import qualified Control.Lens as Lens

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Primitive      (PrimMonad, PrimState, RealWorld)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class    (MonadState(..))
import           Control.Monad.Primitive  (PrimMonad (..))
import           Data.Vector.Unboxed.Mutable (Unbox)
import qualified Data.Vector.Unboxed.Mutable as MUVector
import UnliftIO
import qualified Data.Random.Source           as Source
import Control.Monad.ST
import Control.Monad.Primitive

import Control.Monad.Trans.Reader (ReaderT(..))
import Data.STRef


-- | Environment values with stateful capabilities.
--
class HasRef s e | e -> s where
    ref :: Lens' e (Ref s)

-- | Identity state rference where the Ref is the e
--
instance HasRef s (Ref s) where
    ref = lens id (\_ x -> x)

-- | Abstraction over how to read from and write to a mutable rference
--
data Ref a = Ref !(IO a) !(a -> IO ())


--
-- | Read from a Ref
--
readRef :: MonadIO m => Ref a -> m a
readRef (Ref x _) = liftIO x

-- | Write to a Ref
--
writeRef :: MonadIO m => Ref a -> a -> m ()
writeRef (Ref _ x) = liftIO . x

-- | Modify a Ref
-- This function is subject to change due to the lack of atomic operations
--
modifyRef :: MonadIO m => Ref a -> (a -> a) -> m ()
modifyRef (Ref r w) f = liftIO $ fmap f r >>= w

ioRefToRef :: IORef a -> Ref a
ioRefToRef rf = do
    Ref (readIORef rf)
            (\val -> modifyIORef' rf (\_ -> val))

-- | create a new boxed Ref
--
newRef :: MonadIO m => a -> m (Ref a)
newRef a = do
    ioRefToRef <$> newIORef a








