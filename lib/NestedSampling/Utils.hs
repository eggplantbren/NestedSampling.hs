{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NestedSampling.Utils where

import Control.Monad.Primitive (RealWorld)
import System.Random.MWC as MWC (Gen, uniform)
import System.Random.MWC.Distributions as MWC (standard)

-- Logsumexp
logsumexp :: Double -> Double -> Double
logsumexp a b = log (exp (a - xm) + exp (b - xm)) + xm where
  xm = max a b

-- Logdiffexp
logdiffexp :: Double -> Double -> Double
logdiffexp a b
    | b >= a = 0
    | otherwise = b + log (exp (a - b) - 1.0)

-- Mod
myMod :: Double -> Double -> Double
myMod y x = (y/x - (fromIntegral . floor) (y/x))*x

-- Wrap
wrap :: Double -> (Double, Double) -> Double
wrap x (a, b)
    | x < xmin || x > xmax = myMod (x - xmin) (xmax - xmin) + xmin
    | otherwise            = x
  where
    xmin = min a b
    xmax = max a b

-- My favourite heavy tailed distribution
randh :: Gen RealWorld -> IO Double
randh gen = do
    a <- MWC.standard gen
    b <- MWC.uniform gen
    n <- MWC.standard gen
    return $! transform a b n
  where
    transform a b n =
      let t = a/sqrt (- (log b))
      in  10.0**(1.5 - 3.0*(abs t))*n

