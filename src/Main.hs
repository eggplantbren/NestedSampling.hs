import NestedSampling.Sampler
import NestedSampling.SpikeSlab
import System.Random.MWC hiding (initialize)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    -- Create a sampler with 100 particles, and 1000 MCMC steps per NS iteration
    origin <- initialize 1000 1000 fromPrior logLikelihood gen

    -- Do 100000 NS iterations (this'll go to a depth of 100 nats)
    _ <- nestedSampling 100000 origin gen

    return ()

