-- Do the runs for Syara's project
import Control.Monad
import Control.Monad.Primitive
import Data.Maybe
import Models.Rosenbrock
import NestedSampling.Logging
import NestedSampling.Sampler
import System.Process
import System.IO
import System.Random.MWC

-- Does a run and increments k until it's 100
doRun :: Gen RealWorld -> Int -> Int -> Int -> IO ()
doRun gen k n m
    | k == 100  = return ()
    | otherwise = do
        let maxDepth      = 500.0   :: Double
            numIterations = floor $ maxDepth * fromIntegral n :: Int

        -- Create the sampler
        origin <- NestedSampling.Sampler.initialize
                            n m rosenbrockModel gen

        -- Do NS iterations until maxDepth is reached
        let logging = if n <= 50 then defaultLogging else fromMaybe
                            (error "Error.") (thinnedBy (n `div` 50))
        finalSampler <- nestedSampling logging numIterations origin gen

        -- For the CSV file
        let filename = "outputs/" ++ show n ++ "_" ++ show m ++ ".csv"
        h <- openFile filename (if k==0 then WriteMode else AppendMode)
        when (k==0) (hPutStrLn h "H,logZ")

        hPutStr h $ show $ samplerLogZ finalSampler
        hPutStr h ","
        hPutStr h $ show $ samplerInfo finalSampler
        hPutStrLn h ""
        hFlush h
        hClose h

        -- Create a directory
        let str = "outputs/" ++ show n ++ "_" ++ show m
        _ <- createProcess (proc ("mkdir") [str])

        -- Filename
        let f = "repetition" ++ show k ++ ".csv"
        _ <- createProcess (proc ("mv") ["nested_sampling_info.csv", str ++ "/" ++ f])

        doRun gen (k+1) n m

-- Main
main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do

    -- Set properties of runs
    let numParticles = [1, 2, 5,
                        10, 20, 50,
                        100, 200, 500,
                        1000, 2000, 5000, 10000] :: [Int]
    let mcmcSteps = map (*5) [100000, 50000, 20000,
                     10000, 5000, 2000,
                     1000, 500, 200,
                     100, 50, 20, 10] :: [Int]

    _ <- sequence $ zipWith (doRun gen 0) numParticles mcmcSteps
    return ()

