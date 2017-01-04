import Model.SpikeSlab
import NestedSampling.Logging
import NestedSampling.Sampler
import System.Random.MWC (withSystemRandom, asGenIO)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    -- Create a sampler with 1000 particles and 100 MCMC steps per NS iteration
    origin <- initialize 1000 100 fromPrior logLikelihood perturb gen

    -- Do 100000 NS iterations (this'll go to a depth of 100 nats)
    _ <- nestedSampling noLogging 1000 origin gen

    return ()

