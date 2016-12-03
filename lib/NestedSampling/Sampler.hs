{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NestedSampling.Sampler where

import Control.Monad
import Control.Monad.Primitive (RealWorld)
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import Data.List
import qualified Data.Vector.Unboxed as U
import NestedSampling.SpikeSlab
import NestedSampling.Utils
import System.IO
import System.Random.MWC (Gen)
import qualified System.Random.MWC as MWC

type Particle = U.Vector Double

data Sampler = Sampler {
    numParticles      :: {-# UNPACK #-} !Int
  , mcmcSteps         :: {-# UNPACK #-} !Int
  , theParticles      :: !(IntPSQ Double Particle)
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

metropolisUpdate
  :: Double -> (Double, Particle) -> Gen RealWorld -> IO (Double, Particle)
metropolisUpdate threshold (logL, x) gen = do
    (proposal, logH) <- perturb x gen
    let a = exp logH
    uu <- MWC.uniform gen
    let llProposal = logLikelihood proposal
    let accept = (uu < a) && (llProposal > threshold)
    return $
      if   accept
      then (llProposal, proposal)
      else (logL, x)
{-# INLINE metropolisUpdate #-}

metropolisUpdates
  :: Int -> Double -> (Double, Particle) -> Gen RealWorld
  -> IO (Double, Particle)
metropolisUpdates = loop where
  loop n threshold particle gen
    | n <= 0    = return particle
    | otherwise = do
        next <- metropolisUpdate threshold particle gen
        loop (n - 1) threshold next gen
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
  let it        = iteration sampler
      k         = fromIntegral it
      n         = numParticles sampler
      np        = fromIntegral n
      particles = theParticles sampler
      lz        = logZ sampler
      info      = information sampler
      steps     = mcmcSteps sampler
      logX      = negate $ fromIntegral it / fromIntegral n

      (iworst, lworst, worst) = case PSQ.findMin particles of
        Nothing -> error "nestedSamplingInteration: no particles found"
        Just p  -> p

  copy <- chooseCopy iworst n gen

  -- Approximate log prior weight of the worst particle
  let logPriorWeight = -(k/np) + log (exp (1.0/np) - 1.0)
      logPost        = logPriorWeight + lworst
      logZ'          = logsumexp lz logPost

  let information' =
          exp (logPost - logZ') * lworst
        + exp (lz - logZ') * (info + lz)
        - logZ'

  -- Print some stuff from time to time
  let display = it `mod` n == 0

  when display $ do
    putStr   $ "Iteration " ++ (show it) ++ ". "
    putStr   $ "ln(X) = " ++ (show logX) ++ ". "
    putStrLn $ "ln(L) = " ++ (show lworst) ++ "."
    putStr   $ "ln(Z) = " ++ (show logZ') ++ ", "
    putStrLn $ "H = " ++ (show information') ++ " nats.\n"

  -- Write to file
  writeToFile (it /= 1) (logPriorWeight, lworst) worst

  let particle = case PSQ.lookup copy particles of
        Nothing -> error "nestedSamplingIteration: no particles found"
        Just p  -> p

  -- Do Metropolis
  (newLl, newP) <- metropolisUpdates steps lworst particle gen
  let replace mparticle = case mparticle of
        Just (idx, _, _) -> ((), Just (idx, newLl, newP))
        Nothing          -> ((), Nothing)

  let (_, theParticles') = PSQ.alterMin replace particles

  -- Updated sampler
  let sampler' = Sampler {
          numParticles      = n
        , mcmcSteps         = steps
        , theParticles      = theParticles'
        , iteration         = it + 1
        , logZ              = logZ'
        , information       = information'
        }

  return $! sampler'

