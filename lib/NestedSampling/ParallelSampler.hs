module NestedSampling.ParallelSampler where

-- Imports
import Control.Monad.Primitive
import qualified Data.Vector as V
import NestedSampling.Sampler
import System.Random.MWC

data ParallelSampler a = ParallelSampler
                         {
                             psNumThreads :: !Int,
                             psSamplers   :: !(V.Vector (Sampler a)),
                             psGenerators :: !(V.Vector (Gen RealWorld))
                         }

