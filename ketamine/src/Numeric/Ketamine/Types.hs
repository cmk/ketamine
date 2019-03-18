{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Ketamine.Types (
    Conf (..)
  , view
  , views
  , preview
  , module Export
) where

import GHC.Exts (RealWorld)

--import Control.Lens

import Control.Applicative (Const(..))
import Control.Exception.Safe
import Control.Monad.IO.Class --(MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Primitive  (PrimMonad (..))
import Control.Monad.Reader as Reader
--import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Monoid (First(..))
--import Data.IORef 
import UnliftIO

import Lens.Micro as Export
import Lens.Micro.Type as Export
import Lens.Micro.Internal as Export

newtype AgtState s = AgtState { _agtState :: s }

newtype EntState s = EntState { _entState :: s }

class EnState s e | e -> s where en_state :: Lens' e (AgtState s)

class AgState s a | a -> s where ag_state :: Lens' a (EntState s)

-- TODO re-exports only
class Conf a e | e -> a where conf :: Lens' e a

{- |
'view' is a synonym for ('^.'), generalised for 'MonadReader' (we are able to use it instead of ('^.') since functions are instances of the 'MonadReader' class):

>>> view _1 (1, 2)
1

When you're using 'Reader.Reader' for config and your config type has lenses generated for it, most of the time you'll be using 'view' instead of 'Reader.asks':

@
doSomething :: ('MonadReader' Config m) => m Int
doSomething = do
  thingy        <- 'view' setting1  -- same as “'Reader.asks' ('^.' setting1)”
  anotherThingy <- 'view' setting2
  ...
@
-}
view :: MonadReader s m => Getting a s a -> m a
view l = Reader.asks (getConst #. l Const)
{-# INLINE view #-}

views :: MonadReader s m => LensLike' (Const r) s a -> (a -> r) -> m r
views l f = Reader.asks (getConst #. l (Const #. f))

{- |
'preview' is a synonym for ('^?'), generalised for 'MonadReader' (just like 'view', which is a synonym for ('^.')).

>>> preview each [1..5]
Just 1
-}
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = Reader.asks (getFirst #. foldMapOf l (First #. Just))
{-# INLINE preview #-}


{-
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
-}
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



