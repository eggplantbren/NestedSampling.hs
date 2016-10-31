import NestedSampling.RNG
import System.Random

-- Random number seed for the run
seed :: Int
seed = 123

main = do
    -- Set the seed
    setSeed seed
    putStrLn "# NestedSampling.hs"
    x <- randn 3
    print x
--    y <- perturb 3 x
--    print y

