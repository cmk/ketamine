{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Ketamine.Types where

import GHC.Exts (RealWorld)

import Control.Lens
import Control.Exception.Safe
import Control.Monad.IO.Class --(MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Primitive  (PrimMonad (..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT(..))
--import Data.IORef 
import UnliftIO
import Numeric.Ketamine.Effect.State
import Numeric.Ketamine.Effect.Random

import qualified System.Random.TF             as TF
import qualified System.Random.TF.Instances   as TF



-- | The Reader+IO monad. This is different from a 'ReaderT' because:
--
-- * It's not a transformer, it hardcodes IO for simpler usage and
-- error messages.
--
-- * Instances of typeclasses like 'MonadLogger' are implemented using
-- classes defined on the environment, instead of using an
-- underlying monad. See 'Numeric.Ketamine.Effect.Log'.
newtype Keta e a = Keta { unKeta :: ReaderT e IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader e, MonadThrow)

runKeta :: MonadIO m => e -> Keta e a -> m a
runKeta e (Keta (ReaderT f)) = liftIO (f e)

liftKeta :: (MonadIO m, MonadReader e m) => Keta e a -> m a
liftKeta k = ask >>= (`runKeta` k)


instance MonadUnliftIO (Keta e) where
    askUnliftIO = 
        Keta $ ReaderT $ \r ->
            withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r . unKeta))

instance PrimMonad (Keta e) where
    type PrimState (Keta e) = PrimState IO
    primitive = Keta . ReaderT . const . primitive

{-
newtype KetaST e a = Keta { unKeta :: ReaderT e ST a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader e, MonadThrow)

runKeta :: MonadIO m => e -> Keta e a -> m a
runKeta e (Keta (ReaderT f)) = liftIO (f e)

liftKeta :: (MonadIO m, MonadReader e m) => Keta e a -> m a
liftKeta k = do
    e <- ask
    runKeta e k
-}



