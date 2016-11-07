import NestedSampling.Sampler

main :: IO ()
main = do
    -- Set the seed
    -- setSeed 123

    -- Create and initialise sampler
    sampler <- generateSampler 100 1000

    -- Do an NS iteration
    sampler' <- nestedSamplingIteration sampler
    return ()

