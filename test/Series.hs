{-# OPTIONS_GHC -Wno-orphans #-}

module Series (Series) where

import Data.Series (DataPoint (DataPoint), Series, series)
import Data.Time (UTCTime (UTCTime))
import Data.Time.Calendar (addDays)
import Data.Time.Clock.System (systemEpochDay)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.Tasty.QuickCheck (listOf)

instance Arbitrary UTCTime where
  arbitrary =
    UTCTime
      <$> (addDays <$> arbitrary <*> pure systemEpochDay)
      <*> (fromInteger <$> arbitrary)

instance Arbitrary a => Arbitrary (DataPoint a) where
  arbitrary =
    DataPoint
      <$> arbitrary @UTCTime
      <*> arbitrary @a

instance Arbitrary a => Arbitrary (Series a) where
  arbitrary =
    series
      <$> listOf arbitrary
