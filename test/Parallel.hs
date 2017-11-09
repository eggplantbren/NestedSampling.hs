import Control.Monad.Trans.Maybe
import Models.SpikeSlab
import NestedSampling.Logging
import NestedSampling.ParallelSampler
import System.Random.MWC (withSystemRandom, asGenIO)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do

    -- Set the properties of the run you want to do
    let numParticles  = 100     :: Int
        mcmcSteps     = 1000    :: Int
        maxDepth      = 100.0   :: Double
        numIterations = floor $ maxDepth * fromIntegral numParticles :: Int

    -- Create the sampler
    origin <- runMaybeT $
                makeParallelSampler numParticles mcmcSteps 8 0 spikeslabModel

--    -- Do NS iterations until maxDepth is reached
--    _ <- nestedSampling defaultLogging numIterations origin gen


    return ()

