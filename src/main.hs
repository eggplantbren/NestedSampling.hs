import NestedSampling.RNG
import NestedSampling.SpikeSlab

main = do
    -- Set the seed
    -- setSeed 123
    putStrLn "# NestedSampling.hs"
    putStrLn "# Generating 10 particles from the prior\
                       \ and printing their log likelihoods"
    let actions = [fromPrior | i <- [0..9]]
    particles <- sequence actions
    print $ map logLikelihood particles

