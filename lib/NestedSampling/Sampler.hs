{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NestedSampling.Sampler where

import System.IO
import Control.Monad
import Control.Monad.Primitive (RealWorld)
import Data.List
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import qualified Data.Vector.Unboxed as U
import System.Random.MWC (Gen)
import qualified System.Random.MWC as MWC

import NestedSampling.SpikeSlab
import NestedSampling.Utils

type Particle = U.Vector Double

data Sampler = Sampler {
    numParticles      :: {-# UNPACK #-} !Int
  , mcmcSteps         :: {-# UNPACK #-} !Int
  , theParticles      :: !(IntPSQ Double Particle) -- Particles weighted by loglikelihoods
  , iteration         :: {-# UNPACK #-} !Int
  , logZ              :: {-# UNPACK #-} !Double
  , information       :: {-# UNPACK #-} !Double
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
generateSampler n m gen = do
    putStrLn $ "Generating " ++ show nv ++ " particles from the prior..."
    particles <- replicateM nv (fromPrior gen)
    let lls = fmap logLikelihood particles
    putStrLn "done."
    return Sampler {
        numParticles      = nv
      , mcmcSteps         = mv
      , theParticles      = PSQ.fromList (zip3 [0..] lls particles)
      , iteration         = 1
      , logZ              = -1E300
      , information       = 0.0
      }
  where
    nv = if n <= 1 then 1 else n
    mv = if m <= 0 then 0 else m

-- Find the index and the log likelihood value of the worst particle
findWorstParticle :: Sampler -> (Int, Double, Particle)
findWorstParticle sampler = case worst of
    Just (idx, ll, p) -> (idx, ll, p)
    Nothing           -> error "findWorstParticle: no particles found"
  where
    worst = PSQ.findMin (theParticles sampler)

-- Function to do a single metropolis update
-- Input: A vector of parameters and a loglikelihood threshold
-- Output: An IO action which would return a new vector of parameters
-- and its log likelihood
metropolisUpdate
  :: Double -> (Particle, Double) -> Gen RealWorld -> IO (Particle, Double)
metropolisUpdate threshold (x, logL) gen = do
    (proposal, logH) <- perturb x gen
    let a = exp logH
    uu <- MWC.uniform gen
    let llProposal = logLikelihood proposal
    let accept = (uu < a) && (llProposal > threshold)
    return $
      if   accept
      then (proposal, llProposal)
      else (x, logL)

-- Function to do many metropolis updates
metropolisUpdates :: Int -> Double -> (Particle, Double) -> Gen RealWorld
                                -> IO (Particle, Double)
metropolisUpdates = loop where
  loop n threshold (x, logL) gen
    | n <= 0    = return (x, logL)
    | otherwise = do
                    next  <- metropolisUpdate threshold (x, logL) gen
                    loop (n-1) threshold next gen
{-# INLINE metropolisUpdates #-}

-- -- Do many NestedSampling iterations
nestedSamplingIterations :: Int -> Sampler -> Gen RealWorld -> IO Sampler
nestedSamplingIterations = loop where
  loop n sampler gen
    | n <= 0    = return sampler
    | otherwise = do
        next <- nestedSamplingIteration sampler gen
        loop (n-1) next gen
{-# INLINE nestedSamplingIterations #-}

-- Save a particle to disk
writeToFile :: Bool -> (Double, Double) -> Particle -> IO ()
writeToFile append (logw, logl) particle = do
    -- Open the file
    sampleInfo <- openFile "sample_info.txt"
                        (if append then AppendMode else WriteMode)

    hPutStrLn sampleInfo $ (show logw) ++ " " ++ (show logl)
    hClose sampleInfo

    -- Open the file
    sample <- openFile "sample.txt"
                        (if append then AppendMode else WriteMode)

    let particle' = U.toList particle
    hPutStrLn sample $ foldl' (++) [] $ [show x ++ " " | x <- particle']
    hClose sample

-- Do a single NestedSampling iteration
nestedSamplingIteration :: Sampler -> Gen RealWorld -> IO Sampler
nestedSamplingIteration sampler gen = do
  let it   = iteration sampler
      n    = numParticles sampler
      logX = negate $ (fromIntegral it) / (fromIntegral (numParticles sampler))

      (iWorst, logLike, worst) = findWorstParticle sampler
      particles                = theParticles sampler

  copy <- chooseCopy iWorst n gen

  -- Approximate log prior weight of the worst particle
  let logPriorWeight = -(k/np) + log (exp (1.0/np) - 1.0) where
        k  = fromIntegral it
        np = fromIntegral (numParticles sampler)

  let logPost = logPriorWeight + logLike

  let logZ' = logsumexp (logZ sampler) logPost

  let information' = exp (logPost - logZ') * logLike
                     + exp (logZ sampler - logZ') *
                        (information sampler + logZ sampler) - logZ'

  -- Print some stuff from time to time
  let display = it `mod` (numParticles sampler) == 0 :: Bool
  when display $ do
    putStr   $ "Iteration " ++ (show it) ++ ". "
    putStr   $ "ln(X) = " ++ (show logX) ++ ". "
    putStrLn $ "ln(L) = " ++ (show logLike) ++ "."
    putStr   $ "ln(Z) = " ++ (show logZ') ++ ", "
    putStrLn $ "H = " ++ (show information') ++ " nats.\n"

  -- Write to file
  writeToFile (it /= 1) (logPriorWeight, logLike) worst

  let particle = case PSQ.lookup copy particles of
        Nothing      -> error "nestedSamplingIteration: no particles found"
        Just (ll, p) -> (p, ll)

  -- Do Metropolis
  let update = metropolisUpdates (mcmcSteps sampler) logLike
  (newParticle, newLoglikelihood) <- update particle gen

  let substituteNew mparticle = case mparticle of
        Just (idx, _, _) -> ((), Just (idx, newLoglikelihood, newParticle))
        Nothing          -> ((), Nothing)

  let (_, theParticles') = PSQ.alterMin substituteNew particles

  -- Updated sampler
  let sampler' = Sampler {
          numParticles      = numParticles sampler
        , mcmcSteps         = mcmcSteps sampler
        , theParticles      = theParticles'
        , iteration         = it + 1
        , logZ              = logZ'
        , information       = information'
        }

  return $! sampler'

