import Control.Monad (replicateM)
import NestedSampling.RNG
import NestedSampling.SpikeSlab

main = do
    -- Set the seed
    -- setSeed 123

    -- Print some messages
    putStrLn "# Generating 10 particles from the prior\
                       \ and printing their log likelihoods"

    -- Generate particles from the prior
    particles <- replicateM 10 fromPrior

    -- Apply logLikelihood to each particle
    let lls = map logLikelihood particles

    -- Print the results
    mapM_ print lls

