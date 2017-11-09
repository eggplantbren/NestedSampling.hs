module NestedSampling.ParallelSampler (makeParallelSampler) where

-- Imports
import Control.Monad.Primitive
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import Data.Word
import NestedSampling.Model
import NestedSampling.Sampler
import NestedSampling.Utils
import System.Random.MWC as MWC

-- A type representing parallel sampler (contains many regular samplers)
data ParallelSampler a = ParallelSampler
                         {
                             psNumThreads :: !Word32,
                             psSamplers   :: !(V.Vector (Sampler a)),
                             psGenerators :: !(V.Vector (Gen RealWorld))
                         }

-- A smart constructor
makeParallelSampler :: Int
                    -> Int
                    -> Word32
                    -> Word32
                    -> Model a
                    -> MaybeT IO (ParallelSampler a)
makeParallelSampler numParticles mcmcSteps numThreads firstSeed model
  | numThreads <= 1 = hoistMaybe Nothing
  | otherwise       = do
        let seeds = V.fromList $ map (+firstSeed) [0..(numThreads-1)]
        generators <- lift $ V.mapM (MWC.initialize . V.singleton) seeds
        samplers <- lift $ V.mapM
                    (NestedSampling.Sampler.initialize numParticles mcmcSteps model)
                    generators
        let sampler = ParallelSampler
                        numThreads samplers generators
        return $! sampler

