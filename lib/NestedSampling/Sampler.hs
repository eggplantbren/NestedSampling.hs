{-# LANGUAGE BangPatterns #-}

module NestedSampling.Sampler where

import System.IO (hFlush, stdout)
import Control.Monad (replicateM)
import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC (Gen)
import qualified System.Random.MWC as MWC

import NestedSampling.SpikeSlab
import NestedSampling.RNG
import NestedSampling.Utils

-- A sampler
data Sampler = Sampler
               {
                   numParticles      :: {-# UNPACK #-} !Int,
                   mcmcSteps         :: {-# UNPACK #-} !Int,
                   theParticles      :: !(V.Vector (U.Vector Double)),
                   theLogLikelihoods :: !(U.Vector Double),
                   iteration         :: {-# UNPACK #-} !Int,
                   logZ              :: {-# UNPACK #-} !Double
               } deriving Show

-- | Choose a particle to copy, that isn't number k.
chooseCopy :: Int -> Int -> Gen RealWorld -> IO Int
chooseCopy k n = loop where
  loop prng = do
    index <- MWC.uniformR (0, n - 1) prng
    if   index == k
    then loop prng
    else return index

-- Generate a sampler with n particles and m mcmc steps
generateSampler :: Int -> Int -> Gen RealWorld -> IO Sampler
generateSampler n m gen
    | n <= 1    = undefined
    | m <= 0    = undefined
    | otherwise = do
        putStr ("Generating " ++ (show n) ++ " particles\
                        \ from the prior...")
        hFlush stdout
        theParticles <- V.replicateM n (fromPrior gen)
        let lls = V.map logLikelihood theParticles
        putStrLn "done."
        return Sampler {numParticles=n, mcmcSteps=m,
                        theParticles=theParticles,
                        theLogLikelihoods=U.convert lls, iteration=1,
                        logZ = -1E300}

-- Find the index and the log likelihood value of the worst particle
--
-- NB (jtobin):
--   It may be better to use something like a heap so that we can access the
--   min element in O(1).
findWorstParticle :: Sampler -> (Int, Double)
findWorstParticle sampler = (idx, U.unsafeIndex lls idx)
    where
        idx = U.minIndex lls
        lls = theLogLikelihoods sampler

-- Function to do a single metropolis update
-- Input: A vector of parameters and a loglikelihood threshold
-- Output: An IO action which would return a new vector of parameters
-- and its log likelihood
metropolisUpdate :: Double -> ((U.Vector Double), Double) -> Gen RealWorld
                                -> IO ((U.Vector Double), Double)
metropolisUpdate threshold (x, logL) gen = do
    (proposal, logH) <- perturb x gen
    let a = exp logH
    uu <- rand gen
    let llProposal = logLikelihood proposal
    let accept = (uu < a) && (llProposal > threshold)
    return $
      if   accept
      then (proposal, llProposal)
      else (x, logL)

-- Function to do many metropolis updates
metropolisUpdates :: Int -> Double -> ((U.Vector Double), Double) -> Gen RealWorld
                                -> IO ((U.Vector Double), Double)
metropolisUpdates = loop where
  loop n threshold (x, logL) gen
    | n < 0     = undefined
    | n == 0    = return (x, logL)
    | otherwise = do
                    next  <- metropolisUpdate threshold (x, logL) gen
                    loop (n-1) threshold next gen
{-# INLINE metropolisUpdates #-}

-- Do many NestedSampling iterations
nestedSamplingIterations :: Int -> Sampler -> Gen RealWorld -> IO Sampler
nestedSamplingIterations = loop where
  loop n sampler gen
    | n < 0     = undefined
    | n == 0    = return sampler
    | otherwise = do
                    next <- nestedSamplingIteration sampler gen
                    loop (n-1) next gen
{-# INLINE nestedSamplingIterations #-}

-- Do a single NestedSampling iteration
nestedSamplingIteration :: Sampler -> Gen RealWorld -> IO Sampler
nestedSamplingIteration sampler gen = do
    let worst = findWorstParticle sampler
    putStr $ "Iteration " ++ (show $ iteration sampler) ++ ". "
    putStr $ "log(L) = " ++ (show $ snd worst) ++ ". "
    let iWorst = fst worst
    let n = numParticles sampler
    copy <- chooseCopy iWorst n gen

    -- Approximate log prior weight of the worst particle
    let logWeight = -(k/n) + log (exp (1.0/n) - 1.0) where
          k = fromIntegral (iteration sampler)
          n = fromIntegral (numParticles sampler)
    let logZ' = logsumexp (logZ sampler) (logWeight + logLike) where
          logLike = snd worst
    putStrLn $ "log(Z) = " ++ (show logZ') ++ "."

    -- Copy a surviving particle
    let particle = (V.unsafeIndex (theParticles sampler) copy,
                        U.unsafeIndex (theLogLikelihoods sampler) copy)

    -- Do Metropolis
    let update = metropolisUpdates (mcmcSteps sampler) (snd worst)
    newParticle <- update particle gen

    theParticles' <- do
      mvec <- V.unsafeThaw (theParticles sampler)
      VM.unsafeModify mvec (const (fst newParticle)) iWorst
      V.freeze mvec

    theLogLikelihoods' <- do
      mvec <- U.unsafeThaw (theLogLikelihoods sampler)
      UM.unsafeModify mvec (const (snd newParticle)) iWorst
      U.freeze mvec

    -- Updated sampler
    let !sampler' = Sampler { numParticles=(numParticles sampler),
                     mcmcSteps=(mcmcSteps sampler),
                     theParticles=theParticles',
                     theLogLikelihoods=theLogLikelihoods',
                     iteration=(iteration sampler + 1),
                     logZ = logZ'}

    return $! sampler'

