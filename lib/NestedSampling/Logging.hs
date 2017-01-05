{- Types, variables and functions related to logging. -}

module NestedSampling.Logging (
    -- * logging
    LoggingOptions(..)
  , defaultLogging
  , noLogging
  , thinnedBy
  ) where

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

