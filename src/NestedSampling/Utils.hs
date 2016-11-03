module NestedSampling.Utils where

-- Logsumexp
logsumexp :: [Double] -> Double
logsumexp a = log (sum $ map exp shifted) + xm
                where shifted = map (\x -> x - xm) a;
                           xm = maximum a

-- Mod
myMod :: Double -> Double -> Double
myMod y x = (y/x - (fromIntegral . floor) (y/x))*x

-- Wrap
wrap :: Double -> (Double, Double) -> Double
wrap x (a, b)
        | b <= a            = undefined
        | x >= a && x <= b  = x
        | otherwise         = let min = minimum [a, b];
                                  max = maximum [a, b] in
                                  myMod (x - min) (max - min) + min


