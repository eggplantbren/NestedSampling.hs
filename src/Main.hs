import NestedSampling.RNG
import NestedSampling.Sampler
import Control.Monad (replicateM)
import System.Random.MWC

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    -- Create a sampler with 100 particles, and 1000 MCMC steps per NS iteration
    sampler <- generateSampler 100 1000 gen

    -- Do 10000 NS iterations (this'll go to a depth of 100 nats)
    nestedSamplingIterations 10000 sampler gen

    return ()

