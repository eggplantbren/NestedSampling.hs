import Model.SpikeSlab
import NestedSampling.Logging
import NestedSampling.Sampler
import System.Random.MWC (withSystemRandom, asGenIO)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do

    -- Set the properties of the run you want to do
    let numParticles  = 1000    :: Int
        mcmcSteps     = 1000    :: Int
        maxDepth      = 100.0   :: Double
        numIterations = floor $ maxDepth * fromIntegral numParticles :: Int

    -- Create the sampler
    origin <- initialize numParticles mcmcSteps spikeslabModel gen

    -- Do NS iterations until maxDepth is reached
    _ <- nestedSampling defaultLogging numIterations origin gen


    return ()

