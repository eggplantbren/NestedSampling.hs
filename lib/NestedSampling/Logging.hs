{- Types, variables and functions related to logging. -}
module NestedSampling.Logging (
    -- * logging
    LoggingOptions(..)
  , defaultLogging
  , noLogging
  , numberLogging
  , thinnedBy
  ) where

import Data.Word

data LoggingOptions = LoggingOptions {
    logSamplerFile    :: Maybe FilePath
  , logParametersFile :: Maybe FilePath
  , logProgress       :: Bool
  , logThinning       :: Int
  }

-- | Default logging options for samplers.
defaultLogging :: LoggingOptions
defaultLogging = LoggingOptions {
    logSamplerFile    = Just "nested_sampling_info.csv"
  , logParametersFile = Just "nested_sampling_parameters.csv"
  , logProgress       = True
  , logThinning       = 1
  }

-- | Append a number to filenames.
numberLogging :: Word32 -> LoggingOptions
numberLogging i = LoggingOptions {
    logSamplerFile    = Just $ "parallel-output/nested_sampling_info"
                                    ++ (show i) ++ ".csv"
  , logParametersFile = Just $ "parallel-output/nested_sampling_parameters"
                                    ++ (show i) ++ ".csv"
  , logProgress       = True
  , logThinning       = 1
  }

-- | Quiet (stdout-only) logging options for samplers.
noLogging :: LoggingOptions
noLogging = LoggingOptions {
    logSamplerFile    = Nothing
  , logParametersFile = Nothing
  , logProgress       = True
  , logThinning       = 1
  }

-- Make a LoggingOptions with the specified level of thinning
thinnedBy :: Int -> Maybe LoggingOptions
thinnedBy k
  | k < 1     = Nothing
  | otherwise = Just LoggingOptions
                {
                  logSamplerFile    = Just "nested_sampling_info.csv",
                  logParametersFile = Just "nested_sampling_parameters.csv",
                  logProgress       = True,
                  logThinning       = k
                }

