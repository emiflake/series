{- |
Module     : Data.Series.TimeRange
Maintainer : emi@haskell.fyi
Description: Time range.

Time range.
-}
module Data.Series.TimeRange (
  TimeRange (..),
) where

import Data.Time (UTCTime)

{- | Represents a slice of time with an upper bound and a lower bound.

     @since 0.1.0.0
-}
data TimeRange = TimeRange
  { lowerBound :: !UTCTime
  , upperBound :: !UTCTime
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    , -- | @since 0.1.0.0
      Eq
    )

-- | @since 0.1.0.0
instance Semigroup TimeRange where
  (<>) (TimeRange lbA ubA) (TimeRange lbB ubB) =
    TimeRange (min lbA lbB) (max ubA ubB)
