import NestedSampling.RNG
import NestedSampling.SpikeSlab

main = do
    -- Set the seed
    -- setSeed 123

    -- Print some messages
    putStrLn "# Generating 10 particles from the prior\
                       \ and printing their log likelihoods"

    -- List of IO actions which generate particles from the prior
    let actions = [fromPrior | i <- [0..9]] :: [IO [Double]]

    -- Execute the actions
    particles <- sequence actions

    -- Get the log likelihoods and turn them into strings
    let s = (map show $ map logLikelihood particles) :: [String]

    -- Print them
    putStrLn $ foldl (++) "" $ map (++ "\n") s

