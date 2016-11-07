module NestedSampling.Sampler where

import System.IO (hFlush, stdout)
import Control.Monad (replicateM)

import NestedSampling.SpikeSlab
import NestedSampling.RNG

-- A sampler
data Sampler = Sampler
               {
                   numParticles :: Int,
                   mcmcSteps :: Int,
                   theParticles :: [[Double]],
                   theLogLikelihoods :: [Double],
                   iteration :: Int
               } deriving Show

-- Generate a sampler with n particles and m mcmc steps
generateSampler :: Int -> Int -> IO Sampler
generateSampler n m
    | n <= 0    = undefined
    | m <= 0    = undefined
    | otherwise = do
        putStr ("Generating " ++ (show n) ++ " particles\
                        \ from the prior...")
        hFlush stdout
        theParticles <- replicateM n fromPrior
        let lls = map logLikelihood theParticles
        putStrLn "done."
        return Sampler {numParticles=n, mcmcSteps=m,
                        theParticles=theParticles,
                        theLogLikelihoods=lls, iteration=0}

-- Find the index and the log likelihood value of the worst particle
findWorstParticle :: Sampler -> (Int, Double)
findWorstParticle sampler = (vec !! 0, worst)
    where
        vec = [i | i <- [0..(numParticles sampler - 1)], (l i) == worst]
        l i = (lls) !! i
        worst = minimum lls
        lls = theLogLikelihoods sampler

-- Function to do a single metropolis update
-- Input: A vector of parameters and a loglikelihood threshold
-- Output: An IO action which would return a new vector of parameters
-- and its log likelihood
metropolisUpdate :: Double -> ([Double], Double) -> IO ([Double], Double)
metropolisUpdate threshold (x, logL) = do
    print logL
    (proposal, logH) <- perturb x
    let a = exp logH
    uu <- rand 1
    let u = uu !! 0
    let llProposal = logLikelihood proposal
    let accept = (u < a) && (llProposal > threshold)
    let newParticle = if accept then proposal else x
    let newLogL = if accept then llProposal else logL
    return (newParticle, newLogL)

-- Function to do many metropolis updates
metropolisUpdates :: Int -> Double -> ([Double], Double)
                                -> IO ([Double], Double)
metropolisUpdates n threshold (x, logL)
    | n < 0     = undefined
    | n == 0    = metropolisUpdate threshold (x, logL)
    | otherwise = do
                    result1 <- metropolisUpdate threshold (x, logL)
                    result2 <- metropolisUpdates (n-1) threshold result1
                    return result2

-- Do a NestedSampling iteration
nestedSamplingIteration :: Sampler -> IO Sampler
nestedSamplingIteration sampler = do
    let worst = findWorstParticle sampler
    putStr $ "Iteration " ++ (show $ iteration sampler) ++ ". "
    putStr $ "Log likelihood = " ++ (show $ snd worst)
    return sampler

