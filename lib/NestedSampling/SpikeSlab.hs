module NestedSampling.SpikeSlab where

import Control.Monad (replicateM)
import Control.Monad.Primitive (RealWorld)
import Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import NestedSampling.RNG
import NestedSampling.Utils
import System.Random.MWC (Gen)

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
fromPrior :: Gen RealWorld -> IO (U.Vector Double)
fromPrior gen = do
    x <- U.replicateM 20 (rand gen)
    return $ U.map (\a -> a - 0.5) x

-- Perturb takes a list of doubles as input
-- and returns an IO action that returns the
-- perturbed particle and the logH value.
perturb :: U.Vector Double -> Gen RealWorld -> IO (U.Vector Double, Double)
perturb params gen = do
    -- Choose a parameter to perturb
    k <- randInt (U.length params) gen

    -- Draw from randh
    rh <- randh gen

    -- NB (jtobin):
    --   note that we can't use unsafeThaw here as the params vector could
    --   still be used elsewhere (i.e. in the non-accepting branch of
    --   metropolisUpdate).
    params' <- do
      mvec <- U.thaw params
      UM.unsafeModify mvec (`perturbSingle` rh) k
      U.unsafeFreeze mvec

    return (params', 0.0)

-- Perturb a single double using the provided randh and
-- wrap back into [-0.5, 0.5]
perturbSingle :: Double -> Double -> Double
perturbSingle x rh = (`wrap` (-0.5, 0.5)) $ x + rh

