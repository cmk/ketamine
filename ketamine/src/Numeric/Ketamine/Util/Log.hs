{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Numeric.Ketamine.Util.Log (
    Logger
  , HasLogger (..)
  , newLogger

  , noLogger
  , withStdLogger
  , withMinimalLogger
  , runStdLogging

  , runLogging

  , recordLog
  , logRecord

  , debug
  , info
  , err
  , debugT
  , infoT
  , errT

  , withContext
  , inContext

  , setLevel

  , logException
  , withExceptionLogged

  , Loc
  , getLoc

  , LogFormat (..)

  , LogLevel (..)

  , LogRecordT
  , LogRecord (..)

  , LogField
  , ToLogFieldValue (..)
  , field

  , LogContext
  , ToLogContext (..)

  , (+++)

  , Namespace (..)
  , Process (..)
  ) where

import           Control.Exception (SomeException)
import           Control.Exception.Safe (Exception (..), MonadMask)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ReaderT (..), local)
import           Data.Text (Text)
import           GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import           System.Environment (getProgName)
import           System.Log.FastLogger (defaultBufSize, newStderrLoggerSet, pushLogStrLn, rmLoggerSet)
import           System.Posix.Process (getProcessID)
import           Numeric.Ketamine.Util.Log.Logfmt (fmtString, formatLogfmt)
import           Numeric.Ketamine.Util.Log.Types
import           Numeric.Ketamine.Types
import qualified Control.Exception.Safe as Safe


-- | Fully customisable function for logging. Useful for when the caller has
--   been keeping track of lower level details such as 'LogLevel' and 'Loc'.
--   Prefer the use of 'debug', 'info' and 'err' over this in general.
recordLog ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => IsLogStr msg
  => LogLevel
  -> Maybe Loc
  -> msg
  -> m ()
recordLog sev loc msg = do
  lgr <- view logger
  let
    lvl = view loggerLevel lgr
    ctx = view loggerContext lgr
    fmt = view loggerFmt lgr
    out = view loggerOut lgr
  when (sev >= lvl) $
    liftIO . out $ runLogFormat fmt sev loc ctx msg

------------------------------------------------------------------------------

-- | Takes a 'LogRecord' and records the message with 'LogLevel' and 'Loc',
--   within the 'LogContext' using 'recordLog' and 'inContext'.
logRecord ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => IsLogStr msg
  => LogRecord msg
  -> m ()
logRecord (LogRecord ctx sev loc msg) =
  recordLog sev loc msg `inContext` ctx

------------------------------------------------------------------------------

noLogger :: Logger
noLogger =
  newLogger Err mempty (LogFormat $ \_ _ _ -> mempty) (const $ pure ())

-- | Standard logger for production use
--
-- Adds the current process' name and PID to the 'LogContext'. Log
-- output is buffered and goes to @stderr@.
withStdLogger :: (MonadIO m, MonadMask m) => LogLevel -> (Logger -> m a) -> m a
withStdLogger sev f = Safe.bracket (liftIO stdLogger) (liftIO . snd) (f . fst)
  where
    stdLogger = do
      io <- newStderrLoggerSet defaultBufSize
      prog <- Process <$> getProgName
      pid  <- getProcessID
      let
        ctx = prog +++ pid
        fmt = LogFormat formatLogfmt
        out = pushLogStrLn io

      pure (newLogger sev ctx fmt (liftIO . out), rmLoggerSet io)
    

-- | Very minimal logger. Doesn't print much except errors.
--
-- 'LogContext'. Log output is buffered and goes to @stderr@.
withMinimalLogger 
  :: (MonadIO m, MonadMask m) 
  => LogLevel 
  -> (Logger -> m a) 
  -> m a
withMinimalLogger sev f = 
  Safe.bracket (liftIO stdLogger) (liftIO . snd) (f . fst)
    where
      stdLogger = do
          io <- newStderrLoggerSet defaultBufSize
          let
            fmt = LogFormat (\_ _ _ msg -> fmtString msg)
            out = pushLogStrLn io

          pure (newLogger sev mempty fmt (liftIO . out), rmLoggerSet io)
    

runStdLogging :: (MonadIO m, MonadMask m) => ReaderT Logger m a -> m a
runStdLogging ma = withStdLogger Info $ flip runLogging ma

runLogging :: r -> ReaderT r m a -> m a
runLogging = flip runReaderT

debug ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => IsLogStr msg
  => HasCallStack
  => msg
  -> m ()
debug =
  recordLog Debug getLoc

info ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => IsLogStr msg
  => HasCallStack
  => msg
  -> m ()
info =
  recordLog Info getLoc

err ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => IsLogStr msg
  => HasCallStack
  => msg
  -> m ()
err =
  recordLog Err getLoc

-- | Log a message specialised to 'Text'
debugT ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => HasCallStack
  => Text
  -> m ()
debugT =
  debug

infoT ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => HasCallStack
  => Text
  -> m ()
infoT =
  info

errT ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => HasCallStack
  => Text
  -> m ()
errT =
  err


-- | Augment the 'LogContext' for the given monadic action
--
-- The supplied context is appended to the current one, which is
-- restored afterwards. Fields already present in the current context are
-- overwritten with the supplied ones.
withContext ::
     HasLogger r Logger
  => MonadReader r m
  => ToLogContext c
  => c
  -> m a
  -> m a
withContext ctx =
  local (over (logger . loggerContext) (toLogContext ctx <>))

-- | Flipped version of 'withContext'
--
-- >>> infoT "ohai" `inContext` field "count" (1 :: Int)
inContext ::
     HasLogger r Logger
  => MonadReader r m
  => ToLogContext c
  => m a
  -> c
  -> m a
inContext =
  flip withContext

-- | Adjust the log level for the given monadic action
setLevel :: (HasLogger r Logger, MonadReader r m) => LogLevel -> m a -> m a
setLevel =
  local . set (logger . loggerLevel)

-- | Log 'SomeException' with 'CallStack'
logException ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => Exception e
  => HasCallStack
  => e
  -> m ()
logException e =
  withContext (toException e +++ callStack) $
    errT "An exception occurred."

-- | Run an action and log any exception that might occur.
--
-- The exception is rethrown.
--
-- @ withExceptionLogged ma == Control.Exception.Safe.withException ma logException @
withExceptionLogged ::
     HasLogger r Logger
  => MonadReader r m
  => MonadIO m
  => MonadMask m
  => HasCallStack
  => m a
  -> m a
withExceptionLogged ma =
  Safe.withException ma $ \(e :: SomeException) ->
    logException e

-- | Retrieve a possible 'Loc' value via the 'CallStack'
getLoc :: HasCallStack => Maybe Loc
getLoc =
  case getCallStack callStack of
    [] ->
      Nothing
    xs ->
      Just . toLoc . last $ xs
  where
    toLoc (_, SrcLoc{..}) = Loc
      { loc_filename = srcLocFile
      , loc_package  = srcLocPackage
      , loc_module   = srcLocModule
      , loc_start    = (srcLocStartLine, srcLocStartCol)
      , loc_end      = (srcLocEndLine  , srcLocEndCol  )
      }
