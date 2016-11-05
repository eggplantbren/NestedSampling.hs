import NestedSampling.Sampler

main = do
    -- Set the seed
    -- setSeed 123

    -- Create and print a sampler
    sampler <- generateSampler 10
    print sampler

