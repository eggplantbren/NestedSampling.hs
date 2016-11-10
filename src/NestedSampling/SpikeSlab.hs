module NestedSampling.SpikeSlab where
import NestedSampling.RNG
import NestedSampling.Utils

-- The SpikeSlab model --

-- Log likelihood function
logLikelihood :: [Double] -> Double
logLikelihood params = logsumexp [logl1 + log 100.0, logl2]
    where logl1 = (fromIntegral n)*(c - log u)
                        - 0.5*(sum $ map (\x -> ((x - shift)/u)**2) params)
          logl2 = (fromIntegral n)*(c - log v)
                        - 0.5*(sum $ map (\x -> (x/v)**2) params)
          c = -0.5*log(2*pi)    :: Double
          u = 0.01              :: Double
          v = 0.1               :: Double
          shift = 0.0           :: Double
          n = length params     :: Int

-- fromPrior is an IO action that returns a list of doubles
-- representing a point in the parameter space
fromPrior :: IO [Double]
fromPrior = do
    x <- randList 20
    let y = map (\a -> a - 0.5) x
    return y

-- Perturb takes a list of doubles as input
-- and returns an IO action that returns the
-- perturbed particle and the logH value.
perturb :: [Double] -> IO ([Double], Double)
perturb params = do
    -- Choose a parameter to perturb
    k <- randInt $ length params

    -- Draw from randh
    rh <- randh

    -- Construct proposal vector
    let proposal = [if i==k then
                        (perturbSingle (params !! i) rh)
                        else (params !! i) |
                                i <- [0..(length params - 1)]]
    return (proposal, 0.0)

-- Perturb a single double using the provided randh and
-- wrap back into [-0.5, 0.5]
perturbSingle :: Double -> Double -> Double
perturbSingle x rh = (`wrap` (-0.5, 0.5)) $ x + rh

