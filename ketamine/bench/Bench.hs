{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dsuppress-uniques #-}

-- Previously benchmark include MonadRandom and random-fu directly,
-- both of these have been removed as they are covered by the existing benchmarks.

module Main (main) where

import Criterion      (Benchmark, Benchmarkable, bench, bgroup, whnf, whnfIO)
import Criterion.Main (defaultMain)

import Data.Functor.Identity (Identity)
import qualified Data.Vector.Unboxed as V

-- random

import qualified System.Random as Random

-- mersenne-random-pure64

import qualified System.Random.Mersenne.Pure64 as P64

-- tf-random

import qualified System.Random.TF           as TF
import qualified System.Random.TF.Instances as TF

-- mwc-random

import qualified System.Random.MWC as MWC

-- platform-effect

import qualified Numeric.Ketamine.Util.Random as Platform

main :: IO ()
main = do
    std <- Random.newStdGen
    p64 <- P64.newPureMT
    tf  <- TF.newTFGen
    mwc <- MWC.createSystemRandom

    let iterations = [1]

    defaultMain
        [ steps "int" iterations $ \n run ->
            [ run "stdgen" $
                whnf (uniformStdGen std :: Int -> Int) n
            , run "threefish" $
                whnf (uniformTFGen tf :: Int -> Int) n
            , run "mersenne-pure64" $
                whnf (uniformPureMTInt p64 :: Int -> Int) n
            , run "mwc256" $
                whnfIO (uniformMWC mwc n :: IO Int)
            ]
        , steps "int-seeded" iterations $ \n run ->
            [ run "stdgen" $
                  whnf (\s -> uniformStdGen (Numeric.Ketamine.mkStdGen s) n :: Int)
                       42
            , run "tf-random" $
                  whnf (\s -> uniformTFGen (TF.seedTFGen s) n :: Int)
                      (42,43,44,45)
            , run "mersenne-random-pure64" $
                  whnf (\s -> uniformPureMTInt (P64.pureMT s) n :: Int)
                      42
            -- mersenne-random doesn't support multiple generators per
            -- process.
            , run "mwc256" $ whnfIO $ do
                  g <- MWC.initialize (V.singleton 42)
                  uniformMWC g n :: IO Int
            ]
        , steps "double" iterations $ \n run ->
            [ run "stdgen" $
                whnf (uniformStdGen std :: Int -> Double) n
            , run "mersenne-pure64" $
                whnf (uniformPureMTDouble p64 :: Int -> Double) n
            , run "mwc256" $
                whnfIO (uniformMWC mwc n :: IO Double)
            ]
        , steps "takt-uniform" iterations $ \n run ->
            [ run "stdgen" $
                whnf (taktRandomPure std Numeric.Ketamine.StdUniform :: Int -> Int) n
            , run "mersenne-pure64" $
                whnf (taktRandomPure p64 Numeric.Ketamine.StdUniform :: Int -> Int) n
            , run "devurandom" $
                whnfIO (taktRandom Numeric.Ketamine.DevURandom Numeric.Ketamine.StdUniform n :: IO Int)
            , run "mwc256" $
                whnfIO (taktRandom mwc Numeric.Ketamine.StdUniform n :: IO Int)
            , run "threefish" $
                whnfIO (taktRandom tf Numeric.Ketamine.StdUniform n :: IO Int)
            ]

        , steps "takt-normal" iterations $ \n run ->
            [ run "stdgen" $
                whnf (taktRandomPure std Numeric.Ketamine.StdNormal :: Int -> Double) n
            , run "mersenne-pure64" $
                whnf (taktRandomPure p64 Numeric.Ketamine.StdNormal :: Int -> Double) n
            , run "devurandom" $
                whnfIO (taktRandom Numeric.Ketamine.DevURandom Numeric.Ketamine.StdNormal n :: IO Double)
            , run "mwc256" $
                whnfIO (taktRandom mwc Numeric.Ketamine.StdNormal n :: IO Double)
            , run "threefish" $
                whnfIO (taktRandom tf Numeric.Ketamine.StdNormal n :: IO Double)
            ]
        ]

uniformStdGen :: (Num a, Random.Random a) => Random.StdGen -> Int -> a
uniformStdGen g = countDownGen g Random.random

uniformPureMTInt :: P64.PureMT -> Int -> Int
uniformPureMTInt g = countDownGen g P64.randomInt

uniformPureMTDouble :: P64.PureMT -> Int -> Double
uniformPureMTDouble g = countDownGen g P64.randomDouble

uniformTFGen :: (Num a, TF.Random a) => TF.TFGen -> Int -> a
uniformTFGen g = countDownGen g TF.random

uniformMWC :: (Num a, MWC.Variate a) => MWC.GenIO -> Int -> IO a
uniformMWC g = countDown (MWC.uniform g)

taktRandomPure :: ( Numeric.Ketamine.MonadRandom (Numeric.Ketamine.RandomT g Identity)
                  , Numeric.Ketamine.Distribution d a
                  , Num a
                  )
               => g
               -> d a
               -> Int
               -> a
taktRandomPure g d = Numeric.Ketamine.evalRandom g . countDown (Numeric.Ketamine.getRandom d)

taktRandom :: ( Monad m
              , Numeric.Ketamine.MonadRandom (Numeric.Ketamine.RandomT g m)
              , Numeric.Ketamine.Distribution d a
              , Num a
              )
           => g
           -> d a
           -> Int
           -> m a
taktRandom g d = Numeric.Ketamine.evalRandomT g . countDown (Numeric.Ketamine.getRandom d)

steps :: Show a
      => String
         -- ^ Benchmark group name.
      -> [a]
         -- ^ Benchmark parameters.
      -> (a -> (String -> Benchmarkable -> Benchmark) -> [Benchmark])
         -- ^ A function used to construct a series of benchmarks for each step.
      -> Benchmark
steps group xs f = bgroup group (concatMap go xs)
  where
    go n = f n $ \name g ->
        bench (show n ++ "/" ++ name) g

countDown :: (Monad m, Num a) => m a -> Int -> m a
countDown action limit = go limit 0
  where
    go !n !x
        | n <= 0    = pure x
        | otherwise = do
            y <- action
            go (n - 1) (x + y)

countDownGen :: Num a => g -> (g -> (a, g)) -> Int -> a
countDownGen g action limit = go limit 0 g
  where
    go !n !x gen
        | n <= 0    = x
        | otherwise =
            let (y, gen') = action gen
             in go (n - 1) (x + y) gen'
