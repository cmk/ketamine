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
             OverloadedStrings,
             Rank2Types,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications
#-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Exception.Safe 
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph 
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.IORef
import Numeric.Ketamine.Agent
import Numeric.Ketamine.Environment
import Numeric.Ketamine.Episode
import Numeric.Ketamine.Util.Exception (EpisodeCompleted(..))
import qualified Numeric.Ketamine.Util.Ref as R

import qualified Numeric.Ketamine.Util.Log as L

import qualified Control.Monad.Reader as MTL
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad (void)
import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable
import qualified Data.Primitive.MutVar as M
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Lens.Micro.TH (makeLenses)
import           Text.Printf (printf, PrintfArg(..))

import Numeric.Ketamine.Types
--TODO:  tic tac toe ex


{-

data TestCapabilities = TestCaps
    { _testlogger :: !L.Logger
      , _testState :: (Int,Int) -- ag state / env state
    }
makeLenses ''TestCapabilities

instance L.HasLogger TestCapabilities L.Logger where
  logger = testlogger

type EpState = (Int, Int)

initial :: EpState
initial = (0,0)

--newRef :: PrimMonad m => MutVar (PrimState m) s -> Lens' s a -> Ref (PrimState m) a

test3 :: IO ()
test3 = do 
  epState <- M.newMutVar initial

  let agRef = R.newRef @IO epState _1 
      enRef = R.newRef @IO epState _2
 
  return ()

-- Log the configuration.
info :: (MTL.MonadIO m, L.HasLogger r L.Logger) => r -> Text -> m ()
info caps msg = void . flip MTL.runReaderT caps $ L.infoT msg

getPath
  :: (MonadIO m, L.HasLogger r L.Logger) =>
     r -> Text -> m ()
getPath caps path = info caps . Text.pack $ printf "path is: %s" path

logPath
  :: (L.HasLogger r L.Logger, MTL.MonadReader r m, MonadIO m) =>
     Text -> m ()
logPath path = L.infoT . Text.pack $ printf "path is: %s" path

foo :: (MonadIO m, MonadMask m) => ReaderT TestCapabilities m a -> m a
foo k = L.withStdLogger L.Info $ \l ->
  let 
    caps = TestCaps l initial

  in MTL.runReaderT k caps

bar :: IO ()
bar = foo (logPath "/yo/bitch")
-}

-- TODO move to readme / put in lhs file
-- TODO give example using ether
en :: Int -> EnT Int Int (ReaderT (IORef Int) IO) ()
en = loop where
    loop i = do
         r <- lift ask
         j <- liftIO $ readIORef r
         liftIO $ print $ "en state: " ++ show j
         liftIO $ modifyIORef r $ \k -> k - 1
         if i > 4
             then return ()
             else outcome i >>= loop

-- TODO ignoring input like this is confusing
ag :: AgT Int Int (ReaderT (IORef Int) IO) ()
ag = action 1 >>= loop where
    loop _ = do
         r <- lift ask
         j <- liftIO $ readIORef r
         liftIO $ print $ "ag state: " ++ show j
         liftIO $ modifyIORef r $ \k -> k + 2
         action j >>= loop

test1 :: IO ()
test1 = do
    agr <- newIORef (1 :: Int)
    let rt :: ReaderT (IORef Int) IO ()
        rt = runEpisode $ en +>> ag
    runReaderT rt agr

{-
> test1
"en state: 1"
"ag state: 0"
"en state: 2"
"ag state: 1"
"en state: 3"
"ag state: 2"
"en state: 4"
"ag state: 3"
"en state: 5"
"ag state: 4"
"en state: 6"
"ag state: 5"
"en state: 7"
-}


test2 :: IO ()
test2 = do
    agr <- newIORef (1 :: Int)
    enr <- newIORef (1 :: Int)
    let rt :: ReaderT (IORef Int) (ReaderT (IORef Int) IO) ()
        rt = runEpisode $ en +/> ag 
    (`runReaderT` enr) . (`runReaderT` agr) $ rt

{-
> test2
"en state: 1"
"ag state: 1"
"en state: 0"
"ag state: 3"
"en state: -1"
"ag state: 5"
"en state: -2"
-}


main = test2











