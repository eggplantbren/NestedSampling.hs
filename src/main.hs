import NestedSampling.RNG
import NestedSampling.SpikeSlab

-- Random number seed for the run
seed :: Int
seed = 123

main = do
    -- Set the seed
--    setSeed seed
    putStrLn "# NestedSampling.hs"
    x <- fromPrior
    print $ logLikelihood x

