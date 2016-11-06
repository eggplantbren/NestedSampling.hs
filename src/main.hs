import NestedSampling.Sampler

main = do
    -- Set the seed
    -- setSeed 123

    -- Create and initialise sampler
    sampler <- generateSampler 100 1000

    -- Find worst particle
    let worst = findWorstParticle sampler
    let particle = (theParticles sampler) !! (fst worst)
    let logL = snd worst
    let threshold = snd worst

    -- Do some Metropolis
    print threshold
    metropolisUpdates 10 threshold (particle, logL)

