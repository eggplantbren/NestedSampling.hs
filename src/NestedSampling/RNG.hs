module NestedSampling.RNG where

import System.Random
import Control.Monad

-- To set the seed
setSeed :: Int -> IO ()
setSeed = (setStdGen . mkStdGen)

-- A rand IO action
rand :: IO Double
rand = randomRIO (0 :: Double, 1)

-- A rand function (generates a list)
randList :: Int -> IO [Double]
randList n = replicateM n $ randomRIO (0 :: Double, 1)

-- A random integer
randInt :: Int -> IO Int
randInt k
    | k < 1        = undefined
    | otherwise    = do
                         u <- rand
                         let kk = (fromIntegral k) :: Double
                         return $ floor (kk*u)

-- A randn function
randn :: IO Double
randn = do
              x <- rand
              y <- rand
              return $ boxMuller x y


-- A randn function
randnList :: Int -> IO [Double]
randnList n = do
              x <- randList n
              y <- randList n
              return $ zipWith boxMuller x y

-- My favourite heavy tailed distribution
randhList :: Int -> IO [Double]
randhList m = do
              a <- randnList m
              b <- randList m
              n <- randnList m
              return $ map transform $ zip3 a b n

-- My favourite heavy tailed distribution
randh :: IO Double
randh = do
              a <- randn
              b <- rand
              n <- randn
              return $ transform (a, b, n)

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

