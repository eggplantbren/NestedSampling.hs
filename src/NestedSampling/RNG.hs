module NestedSampling.RNG where

import System.Random
import Control.Monad

-- To set the seed
setSeed :: Int -> IO ()
setSeed = (setStdGen . mkStdGen)

-- A rand function
rand :: Int -> IO [Double]
rand n = replicateM n $ randomRIO (0 :: Double, 1)

-- A randn function
randn :: Int -> IO [Double]
randn n = do
              x <- rand n;
              y <- rand n;
              return $ zipWith boxMuller x y

-- My favourite heavy tailed distribution
randh :: Int -> IO [Double]
randh m = do
              a <- randn m
              b <- rand m
              n <- randn m
              return $ map transform $ zip3 a b n

-- Box-Muller transform used for generating normals
boxMuller :: Double -> Double -> Double
boxMuller u1 u2 = r*(cos theta)
    where
        r = sqrt (-(2*log u1))
        theta = 2*pi*u2

-- Function that transforms (a, b, n) -> x
-- for randh
transform :: (Double, Double, Double) -> Double
transform (a, b, n) = let t = a/sqrt (- (log b)) in 10.0**(1.5 - 3.0*(abs t))*n

