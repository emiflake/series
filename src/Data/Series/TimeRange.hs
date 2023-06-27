module Data.Series.TimeRange (
  TimeRange (..),
) where

import Data.Time (UTCTime)

-- | Represents a slice of time with an upper bound and a lower bound.
data TimeRange = TimeRange
  { lowerBound :: !UTCTime
  , upperBound :: !UTCTime
  }
  deriving stock (Show, Eq)

instance Semigroup TimeRange where
  (<>) (TimeRange lbA ubA) (TimeRange lbB ubB) =
    TimeRange (min lbA lbB) (max ubA ubB)
