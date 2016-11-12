module NestedSampling.SpikeSlab where

import Control.Monad (replicateM)
import Data.Vector.Unboxed as V
import NestedSampling.RNG
import NestedSampling.Utils

-- The SpikeSlab model --

-- Log likelihood function
logLikelihood :: V.Vector Double -> Double
logLikelihood params = logsumexp [logl1 + log 100.0, logl2]
    where logl1 = (fromIntegral n)*(c - log u)
                        - 0.5*(V.sum $ V.map (\x -> ((x - shift)/u)**2) params)
          logl2 = (fromIntegral n)*(c - log v)
                        - 0.5*(V.sum $ V.map (\x -> (x/v)**2) params)
          c = -0.5*log(2*pi)    :: Double
          u = 0.01              :: Double
          v = 0.1               :: Double
          shift = 0.0           :: Double
          n = V.length params   :: Int

-- fromPrior is an IO action that returns a vector of doubles
-- representing a point in the parameter space
fromPrior :: IO (V.Vector Double)
fromPrior = do
    x <- V.replicateM 20 rand
    return $ V.map (\a -> a - 0.5) x

-- Perturb takes a list of doubles as input
-- and returns an IO action that returns the
-- perturbed particle and the logH value.
perturb :: (V.Vector Double) -> IO ((V.Vector Double), Double)
perturb params = do
    -- Choose a parameter to perturb
    k <- randInt $ V.length params

    -- Draw from randh
    rh <- randh

    -- Construct proposal vector
    -- Probably an inefficient method here!
    let params' = V.fromList [if i==k then (perturbSingle (params ! i) rh)
                                else params ! i | i <- [0..(V.length params - 1)]]
    return (params', 0.0)

-- Perturb a single double using the provided randh and
-- wrap back into [-0.5, 0.5]
perturbSingle :: Double -> Double -> Double
perturbSingle x rh = (`wrap` (-0.5, 0.5)) $ x + rh

