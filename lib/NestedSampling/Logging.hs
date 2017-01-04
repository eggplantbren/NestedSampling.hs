{- Types, variables and functions related to logging. -}

module NestedSampling.Logging (
    -- * logging
    LoggingOptions(..)
  , defaultLogging
  , noLogging
  ) where

data LoggingOptions = LoggingOptions {
    logSamplerFile    :: Maybe FilePath
  , logParametersFile :: Maybe FilePath
  , logProgress       :: Bool
  }

-- | Default logging options for samplers.
defaultLogging :: LoggingOptions
defaultLogging = LoggingOptions {
    logSamplerFile    = Just "nested_sampling_info.csv"
  , logParametersFile = Just "nested_sampling_parameters.csv"
  , logProgress       = True
  }

-- | Quiet (stdout-only) logging options for samplers.
noLogging :: LoggingOptions
noLogging = LoggingOptions {
    logSamplerFile    = Nothing
  , logParametersFile = Nothing
  , logProgress       = True
  }

