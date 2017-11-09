module NestedSampling.ParallelSampler (makeParallelSampler) where

-- Imports
import Control.Monad.Primitive
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import Data.Word
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
makeParallelSampler :: Word32 -> Word32 -> MaybeT IO (ParallelSampler a)
makeParallelSampler numThreads firstSeed
  | numThreads <= 1 = hoistMaybe Nothing
  | otherwise       = do
        let seeds = V.fromList $ map (+firstSeed) [0..(numThreads-1)]
        generators <- lift $ V.mapM (MWC.initialize . V.singleton) seeds
        let sampler = ParallelSampler
                        numThreads (V.fromList []) generators
        return $! sampler

