import NestedSampling.RNG
import NestedSampling.Sampler
import Control.Monad (replicateM)

main :: IO ()
main = do
    -- Create a sampler
    sampler <- generateSampler 100 1000

    -- Do 100 NS iterations
    nestedSamplingIterations 100 sampler

    return ()

