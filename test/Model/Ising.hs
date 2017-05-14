
module Model.Ising where

import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC

-- Log likelihood function
logLikelihood :: U.Vector Int -> Double
logLikelihood xs = fromIntegral $ sum terms where
  terms  = map term [0..(n-2)]
  term i = (xs U.! i) * (xs U.! (i+1))
  n      = U.length xs

-- fromPrior is an IO action that returns a vector
-- representing a point in the parameter space
fromPrior :: Gen RealWorld -> IO (U.Vector Int)
fromPrior rng = do
  bits <- U.replicateM 1000 (uniformR (0, 1) rng)
  let xs = U.map (\x -> 2*x-1) bits
  return xs

-- | Perturb a particle, returning the perturbed particle and a logH value.
perturb :: U.Vector Int -> Gen RealWorld -> IO (Double, U.Vector Int)
perturb xs gen = do
    k  <- uniformR (0, U.length xs - 1) gen

    perturbed <- do
      mvec <- U.thaw xs
      UM.unsafeWrite mvec k (-(xs U.! k))
      U.unsafeFreeze mvec

    return (0.0, perturbed)

