{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

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
    samplerDim       :: {-# UNPACK #-} !Int
  , samplerSteps     :: {-# UNPACK #-} !Int
  , samplerParticles :: !(IntPSQ Double Particle)
  , samplerIter      :: {-# UNPACK #-} !Int
  , samplerLogZ      :: {-# UNPACK #-} !Double
  , samplerInfo      :: {-# UNPACK #-} !Double
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
    putStrLn $ "Generating " ++ show samplerDim ++ " particles from the prior..."
    particles <- replicateM samplerDim (fromPrior gen)
    putStrLn "done."
    let lls              = fmap logLikelihood particles
        samplerParticles = PSQ.fromList (zip3 [0..] lls particles)
        samplerIter      = 1
        samplerLogZ      = -1E300
        samplerInfo      = 0
    return Sampler {..}
  where
    samplerDim   = if n <= 1 then 1 else n
    samplerSteps = if m <= 0 then 0 else m

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

-- Do a single NestedSampling samplerIter
nestedSamplingIteration :: Sampler -> Gen RealWorld -> IO Sampler
nestedSamplingIteration Sampler {..} gen = do
  let k = fromIntegral samplerIter
      n = fromIntegral samplerDim
      (iworst, lworst, worst) = case PSQ.findMin samplerParticles of
        Nothing -> error "nestedSamplingInteration: no particles found"
        Just p  -> p

  -- Approximate log prior weight of the worst particle
  let logPriorWeight = - k / n + log (exp (recip n) - 1.0)
      logPost        = logPriorWeight + lworst
      samplerLogZ'   = logsumexp samplerLogZ logPost
      samplerInfo'   =
          exp (logPost - samplerLogZ') * lworst
        + exp (samplerLogZ - samplerLogZ') * (samplerInfo + samplerLogZ)
        - samplerLogZ'

  when (samplerIter `mod` samplerDim == 0) $ do
    putStr   $ "Iteration " ++ (show samplerIter) ++ ". "
    putStr   $ "ln(X) = " ++ show (negate (k / n)) ++ ". "
    putStrLn $ "ln(L) = " ++ (show lworst) ++ "."
    putStr   $ "ln(Z) = " ++ (show samplerLogZ') ++ ", "
    putStrLn $ "H = " ++ (show samplerInfo') ++ " nats.\n"

  -- Write to file
  writeToFile (samplerIter /= 1) (logPriorWeight, lworst) worst

  copy <- chooseCopy iworst samplerDim gen

  let particle = case PSQ.lookup copy samplerParticles of
        Nothing -> error "nestedSamplingIteration: no particles found"
        Just p  -> p

  (newLl, newP) <- metropolisUpdates samplerSteps lworst particle gen

  let replace mparticle = case mparticle of
        Just (idx, _, _) -> ((), Just (idx, newLl, newP))
        Nothing          -> ((), Nothing)

      (_, samplerParticles') = PSQ.alterMin replace samplerParticles

  return $! Sampler {
      samplerParticles = samplerParticles'
    , samplerIter      = samplerIter + 1
    , samplerLogZ      = samplerLogZ'
    , samplerInfo      = samplerInfo'
    , ..
    }

