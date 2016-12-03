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
import qualified Data.Vector.Unboxed.Mutable as UM
import NestedSampling.Utils
import System.IO
import System.Random.MWC (Gen)
import qualified System.Random.MWC as MWC hiding (initialize)
import qualified System.Random.MWC.Distributions as MWC

type Particle  = U.Vector Double
type Particles = IntPSQ Double Particle

data Sampler = Sampler {
    samplerDim        :: {-# UNPACK #-} !Int
  , samplerSteps      :: {-# UNPACK #-} !Int
  , samplerLikelihood :: Particle -> Double
  , samplerParticles  :: !Particles
  , samplerIter       :: {-# UNPACK #-} !Int
  , samplerLogZ       :: {-# UNPACK #-} !Double
  , samplerInfo       :: {-# UNPACK #-} !Double
  }

instance Show Sampler where
  show Sampler {..} = mconcat [
        "Iteration " ++ show samplerIter ++ ". "
      , "ln(X) = " ++ show (negate (k / n)) ++ ". "
      , "ln(L) = " ++ show lworst ++ ".\n"
      , "ln(Z) = " ++ show samplerLogZ ++ ", "
      , "H = " ++ show samplerInfo ++ " nats.\n"
      ]
    where
      k = fromIntegral samplerIter
      n = fromIntegral samplerSteps
      (_, lworst, _) = case PSQ.findMin samplerParticles of
        Nothing -> error "Sampler: no particles"
        Just p  -> p

-- | Initialize a sampler with the provided dimension, number of steps, prior,
--   and log-likelihood.
initialize
  :: Int
  -> Int
  -> (Gen RealWorld -> IO Particle)
  -> (Particle -> Double)
  -> Gen RealWorld
  -> IO Sampler
initialize n m prior logLikelihood gen = do
    putStrLn $ "Generating " ++ show samplerDim ++ " particles from the prior..."
    particles <- replicateM samplerDim (prior gen)
    putStrLn "done."
    let lls               = fmap logLikelihood particles
        samplerParticles  = PSQ.fromList (zip3 [0..] lls particles)
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
  (_, lworst, worst) <- hoistMaybe (PSQ.findMin samplerParticles)
  particles          <- updateParticles Sampler {..} gen

  let k = fromIntegral samplerIter
      n = fromIntegral samplerDim
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
  (iworst, lworst, _) <- hoistMaybe (PSQ.findMin samplerParticles)
  idx      <- lift (chooseCopy iworst samplerDim gen)
  particle <- hoistMaybe (PSQ.lookup idx samplerParticles)
  (ll, p)  <- lift $
    metropolisUpdates samplerSteps lworst particle samplerLikelihood gen

  let replace mparticle = case mparticle of
        Just (j, _, _) -> ((), Just (j, ll, p))
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
    if   index == ref
    then loop prng
    else return $! index

metropolisUpdates
  :: Int
  -> Double
  -> (Double, Particle)
  -> (Particle -> Double)
  -> Gen RealWorld
  -> IO (Double, Particle)
metropolisUpdates = loop where
  loop n threshold particle logLikelihood gen
    | n <= 0    = return particle
    | otherwise = do
        next <- metropolisUpdate threshold particle logLikelihood gen
        loop (n - 1) threshold next logLikelihood gen
{-# INLINE metropolisUpdates #-}

metropolisUpdate
  :: Double
  -> (Double, Particle)
  -> (Particle -> Double)
  -> Gen RealWorld
  -> IO (Double, Particle)
metropolisUpdate threshold (logL, x) logLikelihood gen = do
    (logH, proposal) <- perturb x gen
    let a = exp logH
    uu <- MWC.uniform gen
    let llProposal = logLikelihood proposal
    let accept = (uu < a) && (llProposal > threshold)
    return $
      if   accept
      then (llProposal, proposal)
      else (logL, x)
{-# INLINE metropolisUpdate #-}

-- | Perturb a particle, returning the perturbed particle and a logH value.
perturb :: Particle -> Gen RealWorld -> IO (Double, Particle)
perturb particle gen = do
    k  <- MWC.uniformR (0, U.length particle - 1) gen
    rh <- randh gen

    -- NB (jtobin):
    --   note that we can't use unsafeThaw here as the particle vector could
    --   still be used elsewhere (i.e. in the non-accepting branch of
    --   a Metropolis update).
    perturbed <- do
      mvec <- U.thaw particle
      UM.unsafeModify mvec (`perturbSingle` rh) k
      U.unsafeFreeze mvec

    return (0.0, perturbed)
  where
    perturbSingle :: Double -> Double -> Double
    perturbSingle x rh = (`wrap` (-0.5, 0.5)) $ x + rh

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

-- My favourite heavy tailed distribution
randh :: Gen RealWorld -> IO Double
randh gen = do
    a <- MWC.standard gen
    b <- MWC.uniform gen
    n <- MWC.standard gen
    return $! transform a b n
  where
    transform a b n =
      let t = a/sqrt (- (log b))
      in  10.0**(1.5 - 3.0*(abs t))*n

