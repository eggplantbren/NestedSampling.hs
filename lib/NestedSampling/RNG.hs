module NestedSampling.RNG where

import System.Random.MWC
import Data.Vector.Unboxed as U
import System.Random.MWC.Distributions
import Control.Monad

-- An IO action for generating from U(0, 1)
rand :: IO Double
rand = withSystemRandom . asGenIO $ \gen -> do
          x <- uniform gen :: IO Double
          return x

-- An IO action for generating from U({0, 1, ..., n-1})
randInt :: Int -> IO Int
randInt n
    | n < 1     = undefined
    | otherwise = do
                    u <- rand
                    let nn = (fromIntegral n) :: Double
                    return $ floor (nn*u)

-- An IO action for generating from N(0, 1)
randn :: IO Double
randn = withSystemRandom . asGenIO $ \gen -> do
          x <- standard gen :: IO Double
          return x

-- My favourite heavy tailed distribution
randh :: IO Double
randh = do
            a <- randn
            b <- rand
            n <- randn
            return $ transform (a, b, n)

---- A random integer
--randInt :: Int -> IO Int
--randInt k
--    | k < 1        = undefined
--    | otherwise    = do
--                         u <- rand 1
--                         let kk = (fromIntegral k) :: Double
--                         return $ floor (kk*(u !! 0))

---- A randn function
--randn :: Int -> IO [Double]
--randn n = do
--              x <- rand n
--              y <- rand n
--              return $ zipWith boxMuller x y


---- Box-Muller transform used for generating normals
--boxMuller :: Double -> Double -> Double
--boxMuller u1 u2 = r*(cos theta)
--    where
--        r = sqrt (-(2*log u1))
--        theta = 2*pi*u2

-- Function that transforms (a, b, n) -> x
-- for randh
transform :: (Double, Double, Double) -> Double
transform (a, b, n) = let t = a/sqrt (- (log b)) in 10.0**(1.5 - 3.0*(abs t))*n

