import NestedSampling.Sampler

main = do
    -- Set the seed
    -- setSeed 123

    -- Create and initialise sampler
    sampler <- generateSampler 100

    -- Find worst particle
    let worst = findWorstParticle sampler

    -- Print worst particle (index, log likelihood)
    print worst

