module NestedSampling.RNG where

import Data.Vector.Unboxed as U
import System.Random.MWC (Gen, uniform, uniformR)
import System.Random.MWC.Distributions (standard)
import Control.Monad
import Control.Monad.Primitive (RealWorld)

-- An IO action for generating from U(0, 1)
rand :: Gen RealWorld -> IO Double
rand = uniform

-- An IO action for generating from U({0, 1, ..., n-1})
randInt :: Int -> Gen RealWorld -> IO Int
randInt n = uniformR (0, n - 1)

-- An IO action for generating from N(0, 1)
randn :: Gen RealWorld -> IO Double
randn = standard

-- My favourite heavy tailed distribution
randh :: Gen RealWorld -> IO Double
randh gen = do
            a <- randn gen
            b <- rand gen
            n <- randn gen
            return $! transform (a, b, n)

-- Function that transforms (a, b, n) -> x
-- for randh
transform :: (Double, Double, Double) -> Double
transform (a, b, n) = let t = a/sqrt (- (log b)) in 10.0**(1.5 - 3.0*(abs t))*n

