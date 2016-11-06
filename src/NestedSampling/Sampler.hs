module NestedSampling.Sampler where

import Control.Monad (replicateM)
import NestedSampling.SpikeSlab
import System.IO (hFlush, stdout)
import Data.List.Extras (argmin)

-- A sampler
data Sampler = Sampler
               {
                   numParticles :: Int,
                   theParticles :: [[Double]],
                   theLogLikelihoods :: [Double],
                   iteration :: Int
               } deriving Show

-- Generate a sampler of size n
generateSampler :: Int -> IO Sampler
generateSampler n
    | n <= 0    = undefined
    | otherwise = do
        putStr ("Generating " ++ (show n) ++ " particles\
                        \ from the prior...")
        hFlush stdout
        theParticles <- replicateM n fromPrior
        let lls = map logLikelihood theParticles
        putStrLn "done."
        return Sampler {numParticles=n, theParticles=theParticles,
                            theLogLikelihoods=lls, iteration=0}

-- Find the index and the log likelihood value of the worst particle
findWorstParticle :: Sampler -> (Int, Double)
findWorstParticle sampler = (vec !! 0, worst)
    where
        vec = [i | i <- [0..(numParticles sampler - 1)], (l i) == worst]
        l i = (lls) !! i
        worst = minimum lls
        lls = theLogLikelihoods sampler


