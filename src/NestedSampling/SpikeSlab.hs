module NestedSampling.SpikeSlab where

import Control.Monad (replicateM)
import Data.Vector.Unboxed as U
import NestedSampling.RNG
import NestedSampling.Utils

-- The SpikeSlab model --

-- Log likelihood function
logLikelihood :: U.Vector Double -> Double
logLikelihood params = logsumexp (logl1 + log 100.0) logl2
    where logl1 = (fromIntegral n)*(c - log u)
                        - 0.5*(U.sum $ U.map (\x -> ((x - shift)/u)**2) params)
          logl2 = (fromIntegral n)*(c - log v)
                        - 0.5*(U.sum $ U.map (\x -> (x/v)**2) params)
          c = -0.5*log(2*pi)    :: Double
          u = 0.01              :: Double
          v = 0.1               :: Double
          shift = 0.0           :: Double
          n = U.length params   :: Int

-- fromPrior is an IO action that returns a vector of doubles
-- representing a point in the parameter space
fromPrior :: IO (U.Vector Double)
fromPrior = do
    x <- U.replicateM 20 rand
    return $ U.map (\a -> a - 0.5) x

-- Perturb takes a list of doubles as input
-- and returns an IO action that returns the
-- perturbed particle and the logH value.
perturb :: (U.Vector Double) -> IO ((U.Vector Double), Double)
perturb params = do
    -- Choose a parameter to perturb
    k <- randInt $ U.length params

    -- Draw from randh
    rh <- randh

    let params' = U.generate (U.length params) (\i ->
          if   i == k
          then perturbSingle (params ! i) rh
          else params ! i)

    return (params', 0.0)

-- Perturb a single double using the provided randh and
-- wrap back into [-0.5, 0.5]
perturbSingle :: Double -> Double -> Double
perturbSingle x rh = (`wrap` (-0.5, 0.5)) $ x + rh

