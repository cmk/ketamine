{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module Numeric.Dopamine.Environment.Gym.ClassicControl.PendulumV0 where

import Control.Monad.IO.Class
import Control.Exception.Safe
import Data.Hashable
import GHC.Generics

import Data.Aeson.Types
import OpenAI.Gym (GymEnv(PendulumV0))


-- | State of a PendulumV0 environment
-- see https://gym.openai.com/envs/Pendulum-v0
data State = State
  { cosTheta  :: Float
  , sinTheta  :: Float
  , thetaDot  :: Float
  } deriving (Show, Eq, Generic, Ord, Hashable)


instance FromJSON State where
  parseJSON :: Value -> Parser State
  parseJSON arr@(Array _)= do
    (a, b, c) <- parseJSON arr :: Parser (Float, Float, Float)
    return $ State a b c
  parseJSON invalid = typeMismatch "Environment State" invalid


-- | Force to exert on the pendulum
newtype Action = Action { getAction :: Float }
  deriving (Ord, Show, Eq, Generic, Hashable)


instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . getAction


