module NestedSampling.Model where

-- Imports
import Control.Monad.Primitive
import System.Random.MWC

-- A type for representing models.
-- The type parameter defines the parameter space.
data Model a = Model
               {
                   modelFromPrior :: !(Gen RealWorld -> IO a),
                   modelPerturb   :: !(a -> Gen RealWorld -> IO (Double, a)),
                   modelLogLikelihood :: !(a -> Double)
               }

