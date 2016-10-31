module NestedSampling.RNG where

import System.Random
import Control.Monad

-- A rand function
rand :: Int -> IO [Double]
rand n = replicateM n $ randomRIO (0 :: Double, 1)

-- A randn function
randn :: Int -> IO [Double]
randn n = do
              x <- rand n;
              y <- rand n;
              return $ zipWith boxMuller x y

-- Box-Muller transform
boxMuller :: Double -> Double -> Double
boxMuller u1 u2 = r*(cos theta)
    where
        r = sqrt (-(2*log u1))
        theta = 2*pi*u2


-- To set the seed
setSeed :: Int -> IO ()
setSeed = (setStdGen . mkStdGen)

