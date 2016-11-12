import NestedSampling.RNG
import NestedSampling.Sampler
import Control.Monad (replicateM)
import System.Random.MWC

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    -- Create a sampler
    sampler <- generateSampler 10 1000 gen

    -- Do 100 NS iterations
    nestedSamplingIterations 10000 sampler gen

    return ()

