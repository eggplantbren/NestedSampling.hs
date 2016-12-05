import NestedSampling.Sampler
import NestedSampling.Rosenbrock
import System.Random.MWC hiding (initialize)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    -- Create a sampler with 1000 particles
    -- and 1000 MCMC steps per NS iteration
    origin <- initialize 1000 1000 fromPrior logLikelihood perturb gen

    -- Do 400000 NS iterations (this'll go to a depth of 400 nats)
    _ <- nestedSampling 400000 origin gen

    return ()

