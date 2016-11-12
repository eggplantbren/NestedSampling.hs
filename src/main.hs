import NestedSampling.RNG
import NestedSampling.SpikeSlab
import Control.Monad (replicateM)

main :: IO ()
main = do
    params <- fromPrior

    print params
    result <- perturb params
    let params' = fst result

    print params'

    return ()

