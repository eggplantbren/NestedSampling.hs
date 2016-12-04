module NestedSampling.SpikeSlab where

import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import NestedSampling.Utils
import System.Random.MWC as MWC (Gen, uniform, uniformR)
import System.Random.MWC.Distributions as MWC (standard)

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

-- | Perturb a particle, returning the perturbed particle and a logH value.
perturb :: U.Vector Double -> Gen RealWorld -> IO (Double, U.Vector Double)
perturb particle gen = do
    k  <- MWC.uniformR (0, U.length particle - 1) gen
    rh <- randh gen

    -- NB (jtobin):
    --   note that we can't use unsafeThaw here as the particle vector could
    --   still be used elsewhere (i.e. in the non-accepting branch of
    --   a Metropolis update).
    perturbed <- do
      mvec <- U.thaw particle
      UM.unsafeModify mvec (`perturbSingle` rh) k
      U.unsafeFreeze mvec

    return (0.0, perturbed)
  where
    perturbSingle :: Double -> Double -> Double
    perturbSingle x rh = (`wrap` (-0.5, 0.5)) $ x + rh

-- My favourite heavy tailed distribution
randh :: Gen RealWorld -> IO Double
randh gen = do
    a <- MWC.standard gen
    b <- MWC.uniform gen
    n <- MWC.standard gen
    return $! transform a b n
  where
    transform a b n =
      let t = a/sqrt (- (log b))
      in  10.0**(1.5 - 3.0*(abs t))*n

