import NestedSampling.Sampler
import NestedSampling.SpikeSlab
import System.Random.MWC hiding (initialize)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
  origin <- initialize 1000 1000 fromPrior logLikelihood gen
  _      <- nestedSampling 1000 origin gen

  return ()

