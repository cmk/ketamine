{-# LANGUAGE TypeApplications    #-}

module Test.Hspec.Effect.RandomSpec (spec) where

------------------------------------------------------------------------------
import qualified Data.List.NonEmpty as NE
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QC
------------------------------------------------------------------------------
import Numeric.Ketamine.Capability.Random

spec :: Spec
spec = do
    describe "DiscreteUniform" $ do
        prop "should generate only items contained in the list" $ \seed list ->
            let gen = pureMT seed
                -- QuickCheck now uses its own type NonEmptyList and removes the
                -- instance for NonEmpty. It guarantees that getNonEmpty is a
                -- non-empty list. Thanks QuickCheck...
                neList = QC.getNonEmpty @Int list
                list' = head neList NE.:| tail neList
                d = mkDiscreteUniform' list'
                randD = evalRandom gen $ getRandom d
                contained = elem randD list'
            in QC.counterexample (show randD) contained

        prop "should generate the same results as indexing w/ uniform" $ \seed list ->
            let gen = pureMT seed
                -- See note above
                neList = QC.getNonEmpty @Int list
                list' = head neList NE.:| tail neList
                d = mkDiscreteUniform' list'
                u = Uniform (0 :: Int) $ NE.length list' - 1
                randD = evalRandom gen $ getRandom d
                randU = evalRandom gen $ getRandom u
                equal = randD == list' NE.!! randU
            in QC.counterexample (show (randD,randU)) equal

