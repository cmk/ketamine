{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Ketamine.Util.Clock
    (
    -- * MonadClock
      MonadClock (getPOSIXTime)
    , getCurrentTime
    ) where

import Control.Monad.Trans.Class (MonadTrans (lift))

import Data.Time.Clock       (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)

import qualified Data.Time.Clock.POSIX as POSIX

class Monad m => MonadClock m where
    -- | Get the current POSIX time from the system clock.
    getPOSIXTime :: m POSIXTime

instance MonadClock IO where
    getPOSIXTime = POSIX.getPOSIXTime
    {-# INLINE getPOSIXTime #-}

-- | Any transformer that can lift 'getPOSIXTime' is a valid 'MonadClock' instance.
instance {-# OVERLAPPABLE #-}
    ( MonadTrans t
    , Monad (t m)
    , MonadClock m
    ) => MonadClock (t m)
  where
    getPOSIXTime = lift getPOSIXTime
    {-# INLINE getPOSIXTime #-}

-- | Get the current 'UTCTime'.
getCurrentTime :: MonadClock m => m UTCTime
getCurrentTime = fmap POSIX.posixSecondsToUTCTime getPOSIXTime
{-# INLINE getCurrentTime #-}
