module NestedSampling.SpikeSlab where

import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import NestedSampling.Utils
import System.Random.MWC (Gen, uniform)

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
          shift = 0.031         :: Double
          n = U.length params   :: Int

-- fromPrior is an IO action that returns a vector of doubles
-- representing a point in the parameter space
fromPrior :: Gen RealWorld -> IO (U.Vector Double)
fromPrior gen = do
  x <- U.replicateM 20 (uniform gen)
  return $ U.map (subtract 0.5) x

