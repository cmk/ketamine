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
             Rank2Types,
             ScopedTypeVariables,
             StandaloneDeriving,
             TypeApplications
#-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Applicative (Alternative(..),liftA2)
import Control.Exception.Safe 
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph 
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.IORef
import Numeric.Dopamine.Agent
import Numeric.Dopamine.Environment
import Numeric.Dopamine.Episode
import Numeric.Dopamine.Exception (EpisodeCompleted(..))

--TODO:  tic tac toe ex

main = test2

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














