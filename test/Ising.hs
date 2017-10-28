import Data.Maybe
import Model.Ising
import NestedSampling.Logging
import NestedSampling.Sampler
import System.Random.MWC (withSystemRandom, asGenIO)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do

    -- Set the properties of the run you want to do
    let numParticles  = 1000   :: Int
        mcmcSteps     = 1000  :: Int
        maxDepth      = 500.0 :: Double
        numIterations = floor $ maxDepth * fromIntegral numParticles :: Int

    -- Create the sampler
    origin <- initialize numParticles mcmcSteps isingModel gen

    -- Logging options with thinning
    let loggingOptions = fromMaybe (error "Bad thinning value.")
                                   (thinnedBy 1000)

    -- Do NS iterations until maxDepth is reached
    _ <- nestedSampling loggingOptions numIterations origin gen

    return ()

