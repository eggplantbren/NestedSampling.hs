{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

module NestedSampling.Sampler (
    initialize
  , nestedSampling

  , Sampler(..)
  , Particle
  , Particles
  ) where

import Control.Monad
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import qualified Data.Vector.Unboxed as U
import NestedSampling.Utils
import System.IO
import System.Random.MWC (Gen)
import qualified System.Random.MWC as MWC hiding (initialize)

type Particle  = U.Vector Double

-- Likelihood and tiebreaker
type Lltb = (Double, Double)

type Particles = IntPSQ Lltb Particle
type Perturber = Particle -> Gen RealWorld -> IO (Double, Particle)

data Sampler = Sampler {
    samplerDim        :: {-# UNPACK #-} !Int
  , samplerSteps      :: {-# UNPACK #-} !Int
  , samplerLikelihood :: Particle -> Double
  , samplerPerturber  :: Perturber
  , samplerParticles  :: !Particles
  , samplerIter       :: {-# UNPACK #-} !Int
  , samplerLogZ       :: {-# UNPACK #-} !Double
  , samplerInfo       :: {-# UNPACK #-} !Double
  }

instance Show Sampler where
  show Sampler {..} = mconcat [
        "Iteration " ++ show samplerIter ++ ". "
      , "ln(X) = " ++ show (negate (k / n)) ++ ". "
      , "ln(L) = " ++ show llworst ++ ".\n"
      , "ln(Z) = " ++ show samplerLogZ ++ ", "
      , "H = " ++ show samplerInfo ++ " nats.\n"
      ]
    where
      k = fromIntegral samplerIter
      n = fromIntegral samplerDim
      (_, (llworst, _), _) = case PSQ.findMin samplerParticles of
        Nothing -> error "Sampler: no particles"
        Just p  -> p

-- | Initialize a sampler with the provided dimension, number of steps, prior,
--   perturbation function, and log-likelihood.
initialize
  :: Int                            -- ^ Number of particles
  -> Int                            -- ^ Number of MCMC steps
  -> (Gen RealWorld -> IO Particle) -- ^ Sampling function for prior
  -> (Particle -> Double)           -- ^ Log-likelihood function
  -> Perturber                      -- ^ Perturbation function
  -> Gen RealWorld                  -- ^ PRNG
  -> IO Sampler
initialize n m prior logLikelihood samplerPerturber gen = do
    putStr $ "Generating " ++ show samplerDim ++ " particles from the prior..."
    hFlush stdout
    particles <- replicateM samplerDim (prior gen)
    tbs       <- replicateM samplerDim (MWC.uniform gen)

    putStrLn "done.\n"
    let lls               = fmap logLikelihood particles
        lltbs             = zip lls tbs
        samplerParticles  = PSQ.fromList (zip3 [0..] lltbs particles)
        samplerIter       = 1
        samplerLogZ       = -1E300
        samplerInfo       = 0
        samplerLikelihood = logLikelihood
    return Sampler {..}
  where
    samplerDim   = if n <= 1 then 1 else n
    samplerSteps = if m <= 0 then 0 else m

-- | Perform nested sampling for the specified number of iterations.
nestedSampling :: Int -> Sampler -> Gen RealWorld -> IO Sampler
nestedSampling = loop where
  loop n sampler gen
    | n <= 0    = return sampler
    | otherwise = do
        mnext <- runMaybeT $ nestedSamplingIteration sampler gen
        case mnext of
          Nothing   -> error "nestedSamplingIterations: no particles found"
          Just next -> loop (n - 1) next gen
{-# INLINE nestedSampling #-}

-- | Perform a single nested sampling iteration.
nestedSamplingIteration :: Sampler -> Gen RealWorld -> MaybeT IO Sampler
nestedSamplingIteration Sampler {..} gen = do
  (_, lltbworst, worst) <- hoistMaybe (PSQ.findMin samplerParticles)
  particles            <- updateParticles Sampler {..} gen

  let k = fromIntegral samplerIter
      n = fromIntegral samplerDim
      lworst         = fst lltbworst
      logPriorWeight = - k / n + log (exp (recip n) - 1.0)
      logPost        = logPriorWeight + lworst
      samplerLogZ'   = logsumexp samplerLogZ logPost
      samplerInfo'   =
          exp (logPost - samplerLogZ') * lworst
        + exp (samplerLogZ - samplerLogZ') * (samplerInfo + samplerLogZ)
        - samplerLogZ'

  lift $ do
    when (samplerIter `mod` samplerDim == 0) $ print Sampler {..}
    let iomode = if samplerIter /= 1 then AppendMode else WriteMode
    writeToFile iomode (logPriorWeight, lworst) worst

  return $! Sampler {
      samplerParticles = particles
    , samplerIter      = samplerIter + 1
    , samplerLogZ      = samplerLogZ'
    , samplerInfo      = samplerInfo'
    , ..
    }
{-# INLINE nestedSamplingIteration #-}

updateParticles :: Sampler -> Gen RealWorld -> MaybeT IO Particles
updateParticles Sampler {..} gen = do
  (iworst, lltbworst, _) <- hoistMaybe (PSQ.findMin samplerParticles)
  idx        <- lift (chooseCopy iworst samplerDim gen)
  particle   <- hoistMaybe (PSQ.lookup idx samplerParticles)
  (lltb, p)  <- lift $
    metropolisUpdates
      samplerSteps lltbworst particle samplerLikelihood samplerPerturber gen

  let replace mparticle = case mparticle of
        Just (j, _, _) -> ((), Just (j, lltb, p))
        Nothing        -> ((), Nothing)

      (_, updated) = PSQ.alterMin replace samplerParticles

  return $! updated
{-# INLINE updateParticles #-}

-- | Choose a number in the supplied range that is different from the supplied
--   reference.
chooseCopy :: Int -> Int -> Gen RealWorld -> IO Int
chooseCopy ref n = loop where
  loop prng = do
    index <- MWC.uniformR (0, n - 1) prng
    if   index == ref && n > 1
    then loop prng
    else return $! index

metropolisUpdates
  :: Int
  -> Lltb
  -> (Lltb, Particle)
  -> (Particle -> Double)
  -> Perturber
  -> Gen RealWorld
  -> IO (Lltb, Particle)
metropolisUpdates = loop where
  loop n threshold particle logLikelihood perturber gen
    | n <= 0    = return particle
    | otherwise = do
        next <- metropolisUpdate threshold particle logLikelihood perturber gen
        loop (n - 1) threshold next logLikelihood perturber gen
{-# INLINE metropolisUpdates #-}

metropolisUpdate
  :: Lltb
  -> (Lltb, Particle)
  -> (Particle -> Double)
  -> Perturber
  -> Gen RealWorld
  -> IO (Lltb, Particle)
metropolisUpdate threshold ((ll, tb), x) logLikelihood perturber gen = do
    (logH, proposal) <- perturber x gen
    let a = exp logH
    uu <- MWC.uniform gen
    let llProposal = logLikelihood proposal

    -- Generate tiebreaker for proposal
    rh <- randh gen
    let tbProposal = wrap (tb + rh) (0.0, 1.0)

    -- Check whether proposal is above threshold
    let check1 = llProposal > (fst threshold)
        check2 = llProposal == (fst threshold) &&
                    tbProposal > (snd threshold)

    let accept = (uu < a) && (check1 || check2)
    return $
      if   accept
      then ((llProposal, tbProposal), proposal)
      else ((ll, tb), x)
{-# INLINE metropolisUpdate #-}

-- Save a particle to disk
writeToFile :: IOMode -> (Double, Double) -> Particle -> IO ()
writeToFile mode (logw, logl) particle = do
    sampleInfo <- openFile "sample_info.txt" mode
    hPutStrLn sampleInfo $ (show logw) ++ " " ++ (show logl)
    hClose sampleInfo

    sample <- openFile "sample.txt" mode
    hPutStrLn sample $ U.foldl' (\str x -> str ++ " " ++ show x) [] particle
    hClose sample

-- | Hoist a 'Maybe' into a 'MaybeT'.
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

