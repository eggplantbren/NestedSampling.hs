{-# LANGUAGE RecordWildCards #-}

module NestedSampling.ParallelSampler (makeParallelSampler,
                                       runParallelSampler) where

-- Imports
import Control.Monad.Primitive
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import Data.Word
import NestedSampling.Logging
import NestedSampling.Model
import NestedSampling.Sampler
import NestedSampling.Utils
import System.Random.MWC as MWC

-- A type representing parallel sampler (contains many regular samplers)
data ParallelSampler a = ParallelSampler
                         {
                             psNumThreads :: !Word32,
                             psSamplers   :: !(V.Vector (Sampler a)),
                             psGenerators :: !(V.Vector (Gen RealWorld)),
                             psLogs       :: !(V.Vector LoggingOptions)
                         }

-- A smart constructor
makeParallelSampler :: Int
                    -> Int
                    -> Word32
                    -> Word32
                    -> Model a
                    -> MaybeT IO (ParallelSampler a)
makeParallelSampler numParticles mcmcSteps numThreads firstSeed model
  | numThreads <= 1 = hoistMaybe Nothing
  | otherwise       = do
        -- Create some RNGs
        let seeds = V.fromList $ map (+firstSeed) [0..(numThreads-1)]
        generators <- lift $ V.mapM (MWC.initialize . V.singleton) seeds

        -- Create some logging options, with thread number appended to
        -- filenames
        let loggingOptions = V.fromList $
                                [ numberLogging i | i <- [1..numThreads] ]

        -- The individual samplers
        samplers <- lift $ V.mapM
                    (NestedSampling.Sampler.initialize numParticles mcmcSteps model)
                    generators

        -- Create the parallel sampler
        let sampler = ParallelSampler
                        numThreads samplers generators loggingOptions

        return $! sampler

runParallelSampler :: Show a =>
                      ParallelSampler a -> Int -> IO ()
runParallelSampler (ParallelSampler {..}) numIterations = do
  
    -- Bind (C++ lingo) numIterations to nestedSampling function
    -- i.e., partial currying ?
    let ns logger sampler rng = nestedSampling logger numIterations sampler rng

    -- A bunch of actions, one for each sampler
    let jobs = V.zipWith3 ns psLogs psSamplers psGenerators
    _ <- V.sequence jobs
    return ()

