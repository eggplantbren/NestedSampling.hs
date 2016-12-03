import NestedSampling.Sampler
import System.Random.MWC

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
  sampler <- generateSampler 1000 1000 gen

  _ <- nestedSamplingIterations 1000 sampler gen

  return ()

