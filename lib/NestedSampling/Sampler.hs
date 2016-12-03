{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

module NestedSampling.Sampler where

import Control.Monad
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import qualified Data.Vector.Unboxed as U
import NestedSampling.SpikeSlab
import NestedSampling.Utils
import System.IO
import System.Random.MWC (Gen)
import qualified System.Random.MWC as MWC

type Particle  = U.Vector Double
type Particles = IntPSQ Double Particle

data Sampler = Sampler {
    samplerDim       :: {-# UNPACK #-} !Int
  , samplerSteps     :: {-# UNPACK #-} !Int
  , samplerParticles :: !Particles
  , samplerIter      :: {-# UNPACK #-} !Int
  , samplerLogZ      :: {-# UNPACK #-} !Double
  , samplerInfo      :: {-# UNPACK #-} !Double
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

nestedSamplingIterations :: Int -> Sampler -> Gen RealWorld -> IO Sampler
nestedSamplingIterations = loop where
  loop n sampler gen
    | n <= 0    = return sampler
    | otherwise = do
        mnext <- runMaybeT $ nestedSamplingIteration sampler gen
        case mnext of
          Nothing   -> error "nestedSamplingIterations: no particles found"
          Just next -> loop (n - 1) next gen
{-# INLINE nestedSamplingIterations #-}

-- Save a particle to disk
writeToFile :: IOMode -> (Double, Double) -> Particle -> IO ()
writeToFile mode (logw, logl) particle = do
    sampleInfo <- openFile "sample_info.txt" mode
    hPutStrLn sampleInfo $ (show logw) ++ " " ++ (show logl)
    hClose sampleInfo

    sample <- openFile "sample.txt" mode
    hPutStrLn sample $ U.foldl' (\str x -> str ++ " " ++ show x) [] particle
    hClose sample

nestedSamplingIteration :: Sampler -> Gen RealWorld -> MaybeT IO Sampler
nestedSamplingIteration Sampler {..} gen = do
  (_, lworst, worst) <- hoistMaybe (PSQ.findMin samplerParticles)

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

  particles <- update samplerDim samplerSteps samplerParticles gen

  return $! Sampler {
      samplerParticles = particles
    , samplerIter      = samplerIter + 1
    , samplerLogZ      = samplerLogZ'
    , samplerInfo      = samplerInfo'
    , ..
    }
{-# INLINE nestedSamplingIteration #-}

update :: Int -> Int -> Particles -> Gen RealWorld -> MaybeT IO Particles
update dim steps particles gen = do
  (iworst, lworst, _) <- hoistMaybe (PSQ.findMin particles)
  idx      <- lift (chooseCopy iworst dim gen)
  particle <- hoistMaybe (PSQ.lookup idx particles)
  (ll, p)  <- lift (metropolisUpdates steps lworst particle gen)

  let replace mparticle = case mparticle of
        Just (j, _, _) -> ((), Just (j, ll, p))
        Nothing        -> ((), Nothing)

      (_, updated) = PSQ.alterMin replace particles

  return $! updated
{-# INLINE update #-}

-- | Choose a number in the supplied range that is different from the supplied
--   reference.
chooseCopy :: Int -> Int -> Gen RealWorld -> IO Int
chooseCopy ref n = loop where
  loop prng = do
    index <- MWC.uniformR (0, n - 1) prng
    if   index == ref
    then loop prng
    else return $! index

-- | Hoist a 'Maybe' into a 'MaybeT'.
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

