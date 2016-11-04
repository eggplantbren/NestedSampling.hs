module NestedSampling.SpikeSlab where
import NestedSampling.RNG
import NestedSampling.Utils

-- The SpikeSlab model --

-- Log likelihood function
logLikelihood :: [Double] -> Double
logLikelihood params = logsumexp [100*logl1, logl2]
            where logl1 = -0.5 * (sum $ map (\x -> ((x-shift)/u)**2 + z) params)
                  logl2 = -0.5 * (sum $ map (\x -> (x/v)**2 + z) params)
                  z = log (2*pi) :: Double
                  u = 0.01 :: Double
                  v = 0.1 :: Double
                  shift = 0.03 :: Double

-- Lives in IO and generates parameters from the prior
fromPrior :: IO [Double]
fromPrior = do
                x <- rand 20
                let y = map (\a -> a - 0.5) x
                return y

-- Perturb
perturb :: [Double] -> IO [Double]
perturb params = do
                    -- Choose a parameter to perturb
                    k <- randInt $ length params

                    -- Draw from randh
                    rh <- randh 1

                    -- Construct proposal vector
                    let proposal = [if i==k then
                                        (perturbSingle (params !! i) (rh !! 0))
                                        else (params !! i) |
                                                i <- [0..(length params - 1)]]
                    return proposal

-- Perturb a single double using the provided randh and
-- wrap back into [-0.5, 0.5]
perturbSingle :: Double -> Double -> Double
perturbSingle x rh = (`wrap` (-0.5, 0.5)) $ x + rh

