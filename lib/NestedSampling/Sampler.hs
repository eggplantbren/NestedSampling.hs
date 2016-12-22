{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module NestedSampling.Sampler (
    -- * sampling functions
    initialize
  , nestedSampling

    -- * sampling types
  , Sampler(..)
  , Particles

    -- * logging
  , LoggingOptions(..)
  , defaultLogging
  , noLogging
  ) where

import Control.Monad
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Formatting
import NestedSampling.Utils
import System.IO
import System.Random.MWC (Gen)
import qualified System.Random.MWC as MWC hiding (initialize)

-- Likelihood and tiebreaker
type Lltb = (Double, Double)

type Particles a = IntPSQ Lltb a
type Perturber a = a -> Gen RealWorld -> IO (Double, a)

data Sampler a = Sampler {
    samplerDim        :: {-# UNPACK #-} !Int
  , samplerSteps      :: {-# UNPACK #-} !Int
  , samplerLikelihood :: a -> Double
  , samplerPerturber  :: Perturber a
  , samplerParticles  :: !(Particles a)
  , samplerIter       :: {-# UNPACK #-} !Int
  , samplerLogZ       :: {-# UNPACK #-} !Double
  , samplerInfo       :: {-# UNPACK #-} !Double
  }

instance Show (Sampler a) where
  show Sampler {..} = mconcat [
        "Iteration " ++ show samplerIter ++ ". "
      , "ln(X) = " ++ show (negate (k / n)) ++ ". "
      , "ln(L) = " ++ sllworst ++ ".\n"
      , "ln(Z) = " ++ show samplerLogZ ++ ", "
      , "H = " ++ show samplerInfo ++ " nats.\n"
      ]
    where
      k = fromIntegral samplerIter
      n = fromIntegral samplerDim
      llworst = do
        (_, (llw, _), _) <- PSQ.findMin samplerParticles
        return llw

      sllworst = case llworst of
        Nothing -> "NA"
        Just p  -> show p

-- | Render a Sampler as a text value.
render :: Sampler a -> T.Text
render Sampler {..} =
    sformat
      (int % "," % float % "," % float % "," % string % "," % float % "," % float)
      samplerIter (negate (k / n)) logPriorWeight sllworst samplerLogZ samplerInfo
  where
    k = fromIntegral samplerIter
    n = fromIntegral samplerDim
    logPriorWeight = - k / n + log (exp (recip n) - 1.0)
    llworst = do
      (_, (llw, _), _) <- PSQ.findMin samplerParticles
      return llw

    sllworst = case llworst of
      Nothing -> "NA"
      Just p  -> show p

data LoggingOptions = LoggingOptions {
    logSamplerFile    :: Maybe FilePath
  , logProgress       :: Bool
  }

-- | Default logging options for samplers.
defaultLogging :: LoggingOptions
defaultLogging = LoggingOptions {
    logSamplerFile    = Just "nested_sampling_info.csv"
  , logProgress       = True
  }

-- | Quiet (stdout-only) logging options for samplers.
noLogging :: LoggingOptions
noLogging = LoggingOptions {
    logSamplerFile    = Nothing
  , logProgress       = True
  }

-- | Initialize a sampler with the provided dimension, number of steps, prior,
--   perturbation function, and log-likelihood.
initialize
  :: Int                            -- ^ Number of particles
  -> Int                            -- ^ Number of MCMC steps
  -> (Gen RealWorld -> IO a)        -- ^ Sampling function for prior
  -> (a -> Double)                  -- ^ Log-likelihood function
  -> Perturber a                    -- ^ Perturbation function
  -> Gen RealWorld                  -- ^ PRNG
  -> IO (Sampler a)
initialize n m prior logLikelihood samplerPerturber gen = do
    particles <- replicateM samplerDim (prior gen)
    tbs       <- replicateM samplerDim (MWC.uniform gen)

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
nestedSampling
  :: LoggingOptions -> Int -> Sampler a -> Gen RealWorld -> IO (Sampler a)
nestedSampling logopts = loop where
  loop n sampler gen
    | n <= 0    = return sampler
    | otherwise = do
        mnext <- runMaybeT $ nestedSamplingIteration logopts sampler gen
        case mnext of
          Nothing   -> error "nestedSamplingIterations: no particles found"
          Just next -> loop (n - 1) next gen
{-# INLINE nestedSampling #-}

-- | Perform a single nested sampling iteration.
nestedSamplingIteration
  :: LoggingOptions -> Sampler a -> Gen RealWorld -> MaybeT IO (Sampler a)
nestedSamplingIteration LoggingOptions {..} Sampler {..} gen = do
  (_, lltbworst, _) <- hoistMaybe (PSQ.findMin samplerParticles)
  particles <- updateParticles Sampler {..} gen

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
    when (logProgress && (samplerIter `mod` samplerDim == 0)) $
      print Sampler {..}
    let iomode = if samplerIter /= 1 then AppendMode else WriteMode
    writeToFile LoggingOptions {..} iomode Sampler {..}

  return $! Sampler {
      samplerParticles = particles
    , samplerIter      = samplerIter + 1
    , samplerLogZ      = samplerLogZ'
    , samplerInfo      = samplerInfo'
    , ..
    }
{-# INLINE nestedSamplingIteration #-}

updateParticles :: Sampler a -> Gen RealWorld -> MaybeT IO (Particles a)
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
  -> (Lltb, a)
  -> (a -> Double)
  -> Perturber a
  -> Gen RealWorld
  -> IO (Lltb, a)
metropolisUpdates = loop where
  loop n threshold particle logLikelihood perturber gen
    | n <= 0    = return particle
    | otherwise = do
        next <- metropolisUpdate threshold particle logLikelihood perturber gen
        loop (n - 1) threshold next logLikelihood perturber gen
{-# INLINE metropolisUpdates #-}

metropolisUpdate
  :: Lltb
  -> (Lltb, a)
  -> (a -> Double)
  -> Perturber a
  -> Gen RealWorld
  -> IO (Lltb, a)
metropolisUpdate threshold ((ll, tb), x) logLikelihood perturber gen = do
    (logH, proposal) <- perturber x gen
    let a = exp logH
    uu <- MWC.uniform gen
    let llProposal = logLikelihood proposal

    -- Generate tiebreaker for proposal
    rh <- randh gen
    let !tbProposal = wrap (tb + rh) (0.0, 1.0)

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

-- | Write sampler/particle information to disk.
writeToFile :: LoggingOptions -> IOMode -> Sampler a -> IO ()
writeToFile LoggingOptions {..} mode sampler =
    case logSamplerFile of
      Nothing   -> return ()
      Just file -> do
        sampleInfo <- openFile file mode
        when (mode == WriteMode) $
          T.hPutStrLn sampleInfo "n,ln_x,ln_prior_weight,ln_l,ln_z,h"

        T.hPutStrLn sampleInfo $ render sampler
        hClose sampleInfo

-- | Hoist a 'Maybe' into a 'MaybeT'.
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

