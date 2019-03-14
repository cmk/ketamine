{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Ketamine.Capability.Log.Types (
    LogFormat (..)

  , Logger
  , HasLogger (..)
  , newLogger
  , loggerLevel
  , loggerContext
  , loggerFmt
  , loggerOut

  , Severity (..)
  , severityToLogStr

  , LogRecordT
  , LogRecord (..)

  , LogField
  , LogFieldValue (..)
  , ToLogFieldValue (..)
  , field

  , LogContext (..)
  , ToLogContext (..)

  , (+++)
  , lookupField
  , foldContext

  , Namespace (..)
  , Process (..)

  , LogStr' (..)
  , IsLogStr (logStr)

      -- * Re-exports
  , Loc (..)
  , ToLogStr (..)
  , LogStr
  , logStrLength
  ) where

import Control.Applicative
import           Control.Concurrent (ThreadId)
import           Control.Exception (SomeException)
import           Control.Lens (Lens', makeLenses)
import Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.Map.Strict (Map, singleton)
import qualified Data.Map.Strict as Map
import           Data.Scientific (Scientific)
import           Data.Semigroup (Semigroup)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Word (Word64, Word32, Word16, Word8)
import Data.Int (Int64, Int32, Int16, Int32, Int8)

import           GHC.Stack (CallStack, prettyCallStack)

import           Language.Haskell.TH.Syntax (Loc (..))




import           System.Log.FastLogger (LogStr, ToLogStr (..), logStrLength)
import           System.Posix.Types (CPid (..), ProcessID)

data Severity =
    Debug
  -- ^ Tracing of local execution.
  | Info
  -- ^ Anything providing context for troubleshooting
  | Err
  -- ^ Error conditions
    deriving (Eq, Show, Ord, Enum, Bounded)


renderSeverity :: Severity -> Text
renderSeverity severity =
  case severity of
    Debug ->
      "debug"
    Info ->
      "info"
    Err ->
      "err"


newtype LogFormat =
  LogFormat {
      runLogFormat :: forall msg. IsLogStr msg => Severity -> Maybe Loc -> LogContext -> msg -> LogStr
    }

data Logger =
  Logger {
      _loggerLevel :: !Severity
    , _loggerContext :: !LogContext
    , _loggerFmt :: !LogFormat
    , _loggerOut :: LogStr -> IO ()
    }

class HasLogger s a | s -> a where
  logger :: Lens' s a

instance HasLogger Logger Logger where
  logger = id
  {-# INLINE logger #-}

newLogger ::
     Severity
  -> LogContext
  -> LogFormat
  -> (LogStr -> IO ())
  -> Logger
newLogger =
  Logger

severityToLogStr :: Severity -> LogStr
severityToLogStr =
  fromString . T.unpack . renderSeverity

------------------------------------------------------------------------------

-- | Alias for a 'LogRecord' specialised to 'Text'
type LogRecordT = LogRecord Text

------------------------------------------------------------------------------

-- | Data type for keeping track of log records in a pure context
data LogRecord m = LogRecord LogContext Severity (Maybe Loc) m

------------------------------------------------------------------------------

type LogField = (Text, LogFieldValue)

data LogFieldValue =
    StringV !Text
  | NumV !Scientific
  | BoolV !Bool
    deriving Show

class ToLogFieldValue a where
  toLogFieldValue :: a -> LogFieldValue

instance ToLogFieldValue LogFieldValue where
  toLogFieldValue = id

instance ToLogFieldValue Text where toLogFieldValue = StringV
instance ToLogFieldValue String where toLogFieldValue = StringV . T.pack
instance ToLogFieldValue Scientific where toLogFieldValue = NumV
instance ToLogFieldValue Int where toLogFieldValue = NumV . fromIntegral
instance ToLogFieldValue Integer where toLogFieldValue = NumV . fromInteger
instance ToLogFieldValue Float where toLogFieldValue = NumV . realToFrac
instance ToLogFieldValue Double where toLogFieldValue = NumV . realToFrac
instance ToLogFieldValue Rational where toLogFieldValue = NumV . fromRational
instance ToLogFieldValue Int8 where toLogFieldValue = NumV . fromIntegral
instance ToLogFieldValue Int16 where toLogFieldValue = NumV . fromIntegral
instance ToLogFieldValue Int32 where toLogFieldValue = NumV . fromIntegral
instance ToLogFieldValue Int64 where toLogFieldValue = NumV . fromIntegral
instance ToLogFieldValue Word8 where toLogFieldValue = NumV . fromIntegral
instance ToLogFieldValue Word16 where toLogFieldValue = NumV . fromIntegral
instance ToLogFieldValue Word32 where toLogFieldValue = NumV . fromIntegral
instance ToLogFieldValue Word64 where toLogFieldValue = NumV . fromIntegral

instance ToLogFieldValue SomeException where
  toLogFieldValue = toLogFieldValue . show

instance ToLogFieldValue CallStack where
  toLogFieldValue = toLogFieldValue . prettyCallStack

instance ToLogFieldValue ThreadId where
  toLogFieldValue = toLogFieldValue . show

instance ToLogFieldValue ProcessID where
  toLogFieldValue (CPid pid) = toLogFieldValue pid

-- TODO: HTTP method, response status, time, ... ?

field :: ToLogFieldValue v => Text -> v -> LogField
field k =
  (,) k . toLogFieldValue


-- | A set of key-value pairs providing context
newtype LogContext =
  LogContext {
      fromLogContext :: Map Text LogFieldValue
    } deriving (Monoid, Semigroup, Show)

--
-- known context field names
--
nAMESPACE :: Text
nAMESPACE =
  "ns"

pROCESS :: Text
pROCESS =
  "prog"

pID :: Text
pID =
  "pid"

eXCEPTION :: Text
eXCEPTION =
  "exception"

cALLSTACK :: Text
cALLSTACK =
  "callstack"

tHREADID :: Text
tHREADID =
  "thread.id"

class ToLogContext a where
  toLogContext :: a -> LogContext

instance ToLogContext LogContext where
  toLogContext = id

instance ToLogContext LogField where
  toLogContext = LogContext . uncurry singleton

-- | An arbitrary namespace for the current 'LogContext'
--
-- Useful eg. to namespace logging of 3rd party libraries
newtype Namespace =
  Namespace Text

instance ToLogContext Namespace where
  toLogContext (Namespace x) =
    toLogContext $ field nAMESPACE (toLogFieldValue x)

-- | The process name as obtained by 'System.Environment.getProgName'
newtype Process =
  Process String

instance ToLogContext Process where
  toLogContext (Process x) =
    toLogContext $ field pROCESS (toLogFieldValue x)

instance ToLogContext ProcessID where
  toLogContext = toLogContext . field pID . toLogFieldValue

instance ToLogContext SomeException where
  toLogContext = toLogContext . field eXCEPTION . toLogFieldValue

instance ToLogContext CallStack where
  toLogContext = toLogContext . field cALLSTACK . toLogFieldValue

instance ToLogContext ThreadId where
  toLogContext = toLogContext . field tHREADID . toLogFieldValue

-- | Augment the log context
--
-- This is right-biased, ie. any fields on 'b' overwrite those already present
-- in 'a'.
(+++) :: (ToLogContext a, ToLogContext b) => a -> b -> LogContext
a +++ b = toLogContext b <> toLogContext a
infixl 5 +++

lookupField :: Text -> LogContext -> Maybe LogField
lookupField k =
  fmap ((,) k) . Map.lookup k . fromLogContext

foldContext :: (a -> Text -> LogFieldValue -> a) -> a -> LogContext -> a
foldContext f z =
  Map.foldlWithKey' f z . fromLogContext

data LogStr' =
    LogStrT !Text
  | LogStrLT !TL.Text
  | LogStrS !String
  | LogStrBS !ByteString
  | LogStrLBS !LB.ByteString

class IsLogStr a where
  logStr :: a -> LogStr'

instance IsLogStr Text where logStr = LogStrT
instance IsLogStr TL.Text where logStr = LogStrLT
instance IsLogStr String where logStr = LogStrS
instance IsLogStr ByteString where logStr = LogStrBS
instance IsLogStr LB.ByteString where logStr = LogStrLBS

makeLenses ''Logger
