module NestedSampling.RNG where

import Data.Vector.Unboxed as U
import System.Random.MWC (Gen, uniform, uniformR)
import System.Random.MWC.Distributions (standard)
import Control.Monad
import Control.Monad.Primitive (RealWorld)

-- My favourite heavy tailed distribution
randh :: Gen RealWorld -> IO Double
randh gen = do
            a <- standard gen
            b <- uniform gen
            n <- standard gen
            return $! transform a b n

-- Function that transforms (a, b, n) -> x
-- for randh
transform :: Double -> Double -> Double -> Double
transform a b n =
  let t = a/sqrt (- (log b))
  in  10.0**(1.5 - 3.0*(abs t))*n

