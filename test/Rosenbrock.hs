import Data.Maybe
import Models.Rosenbrock
import NestedSampling.Logging
import NestedSampling.Sampler
import System.Random.MWC hiding (initialize)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do

    -- Set the properties of the run you want to do
    let numParticles  = 500    :: Int
        mcmcSteps     = 10000    :: Int
        maxDepth      = 500.0    :: Double
        numIterations = floor $ maxDepth * fromIntegral numParticles :: Int

    -- Create the sampler
    origin <- initialize numParticles mcmcSteps rosenbrockModel gen

    -- Logging options with thinning
    let loggingOptions = fromMaybe (error "Bad thinning value.") (thinnedBy 100)

    -- Do NS iterations until maxDepth is reached
    _ <- nestedSampling loggingOptions numIterations origin gen

    return ()

