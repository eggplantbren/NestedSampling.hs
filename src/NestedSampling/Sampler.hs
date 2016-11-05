module NestedSampling.Sampler where

import Control.Monad (replicateM)
import NestedSampling.SpikeSlab

-- A sampler
data Sampler = Sampler
               {
                   numParticles :: Int,
                   theParticles :: [[Double]],
                   theLogLikelihoods :: [Double]
               } deriving Show

-- Generate a sampler of size n
generateSampler :: Int -> IO Sampler
generateSampler n
    | n <= 0    = undefined
    | otherwise = do
        theParticles <- replicateM n fromPrior
        let lls = map logLikelihood theParticles
        return Sampler {numParticles=n, theParticles=theParticles,
                            theLogLikelihoods=lls}

