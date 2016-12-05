module NestedSampling.Rosenbrock where

import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import NestedSampling.Utils
import System.Random.MWC as MWC (Gen, uniform, uniformR)

-- A multivariate Rosenbrock likelihood
-- c.f. the one in DNest4

-- Log likelihood function
logLikelihood :: U.Vector Double -> Double
logLikelihood x = -(2.0*f)
    where f      = (sum terms1) + (sum terms2)
          terms1 = [100.0*(x U.! (i+1) - (x U.! i)**2)**2 | i <- [0..(n-2)]]
          terms2 = [(1.0 - (x U.! i))**2 | i <- [0..(n-2)]]
          n      = U.length x :: Int

-- fromPrior is an IO action that returns a point in the parameter space
fromPrior :: Gen RealWorld -> IO (U.Vector Double)
fromPrior gen = do
  x <- U.replicateM 50 (uniform gen)
  return $ U.map (\x' -> -10.0 + 20.0*x') x

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
    perturbSingle x rh = (`wrap` (-10.0, 10.0)) $ x + 20.0*rh

