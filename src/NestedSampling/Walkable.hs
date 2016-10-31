module NestedSampling.Walkable where

-- Define the 'walkable' typeclass
-- for things that can be MCMC'd
class Walkable a where
    perturb :: a -> a


