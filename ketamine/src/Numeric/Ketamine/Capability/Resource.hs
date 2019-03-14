{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.Ketamine.Capability.Resource
    ( module Resource
    )
where

import Control.Monad.Trans.Lift.Local
import Control.Monad.Trans.Resource   as Resource

instance LiftLocal Resource.ResourceT where
    liftLocal _ l = Resource.transResourceT . l
