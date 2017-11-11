import Control.Monad.Trans.Maybe
import Models.SpikeSlab
import NestedSampling.ParallelSampler

main :: IO ()
main = do

    -- First rng seed
    let firstSeed = 0
    let numThreads = 4

    -- Set the properties of the run you want to do
    let numParticles  = 100     :: Int
        mcmcSteps     = 1000    :: Int
        maxDepth      = 100.0   :: Double
        numIterations = floor $ maxDepth * fromIntegral numParticles :: Int

    -- Create the ParallelSampler
    origin <- runMaybeT $
                makeParallelSampler numParticles mcmcSteps
                                    numThreads firstSeed spikeslabModel

    -- Do NS iterations until maxDepth is reached
    _ <- case origin of
              Nothing -> return ()
              Just s  -> runParallelSampler s numIterations

    return ()

