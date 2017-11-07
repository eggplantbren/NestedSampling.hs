{-# LANGUAGE RecordWildCards #-}

module Models.Ising where

import Control.Monad.Primitive (RealWorld)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import NestedSampling.Model
import System.Random.MWC

-- The shape of the system
m :: Int
m = 100

n :: Int
n = 100

-- The size of the system
size :: Int
size = m*n

-- A type representing a state of an Ising model
-- and an integer proportional to its energy
data IsingState = IsingState
                  {
                      spins  :: !(U.Vector Int),
                      energy :: !Int
                  }

-- show just shows spins
instance Show IsingState where
    show (IsingState spins _) = show spins

-- Access an element
element :: IsingState -> Int -> Int -> Int
element IsingState {..} i j =
  let
    k = i*n + j
  in
    spins U.! k


-- Log likelihood function
logLikelihood :: IsingState -> Double
logLikelihood (IsingState _ energy) = negate $ fromIntegral energy


-- fromPrior is an IO action that returns a vector
-- representing a point in the parameter space
fromPrior :: Gen RealWorld -> IO IsingState
fromPrior rng = do

    -- Generate the state
    bits <- U.replicateM size (uniformR (0, 1) rng)
    let xs = U.map (\x -> 2*x-1) bits

    -- A temporary IsingState with wrong energy
    let temp = IsingState xs 0

    -- Products of left-right neighbours, row i
    let row i = [ (element temp i j) * (element temp i (j+1))
                        | j <- [0..(n-2)] ]
    let rowSum i = sum $ row i

    -- Products of up-down neighbours, column j
    let col j = [ (element temp i j) * (element temp (i+1) j+1)
                        | i <- [0..(m-2)] ]
    let colSum j = sum $ col j

    -- Totals
    let e1 = sum $ [rowSum i | i <- [0..(m-1)]]
    let e2 = sum $ [colSum j | j <- [0..(n-1)]]
    let energy = -(e1 + e2)

    return $! IsingState xs energy

-- | Perturb a particle, returning the perturbed particle and a logH value.
perturb :: IsingState -> Gen RealWorld -> IO (Double, IsingState)
perturb old@(IsingState {..}) gen = do

    -- Which site to flip
    i <- uniformR (0, m-1) gen
    j <- uniformR (0, n-1) gen
    let k = i*n + j

    -- Perturbed spins
    spins' <- do
        mvec <- U.thaw spins
        UM.unsafeWrite mvec k (-(spins U.! k))
        U.unsafeFreeze mvec

    -- Temporary IsingState
    let new = IsingState spins' 0

    -- Product of position (i, j) with neighbours
    let up isingState = if i == 0 then 0 else (element isingState i j)*
                                              (element isingState (i-1) j)
    let down isingState = if i == (m-1) then 0 else (element isingState i j)*
                                                    (element isingState (i+1) j)
    let left isingState = if j == 0 then 0 else (element isingState i j)*
                                                (element isingState i (j-1))
    let right isingState = if j == (n-1) then 0 else (element isingState i j)*
                                              (element isingState i (j+1))

    -- Change in energy
    let delta = up new + down new + left new + right new -
                up old - down old - left old - right old

    return (0.0, IsingState spins' (energy - delta))


isingModel :: Model IsingState
isingModel = Model fromPrior perturb logLikelihood

