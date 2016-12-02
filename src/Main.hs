import NestedSampling.Sampler
import System.Random.MWC

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    -- Create a sampler with 100 particles, and 1000 MCMC steps per NS iteration
    sampler <- generateSampler 1000 1000 gen

    -- Do 100000 NS iterations (this'll go to a depth of 100 nats)
    _ <- nestedSamplingIterations 100000 sampler gen

    return ()

