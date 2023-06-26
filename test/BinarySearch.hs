{-# OPTIONS_GHC -Wno-orphans #-}
module BinarySearch (props) where

import Test.Tasty (TestTree, adjustOption)
import Test.QuickCheck (Property, Arbitrary (arbitrary))
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests (..), QuickCheckMaxSize (..))
import Test.QuickCheck (property)
import Data.Series.Internal (binarySearch, linearSearch)
import Data.Time (UTCTime(..))
import Data.Time.Calendar (addDays)
import Data.Time.Clock.System (systemEpochDay)
import Data.Series (series, DataPoint(..), Series)
import Test.QuickCheck (listOf)

props :: [TestTree]
props =
  [ adjustOption (\_ -> QuickCheckTests 20000) $ adjustOption (\_ -> QuickCheckMaxSize 10000) $ testProperty "binarySearchValid" binarySearchValidProperty
  ]

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

prop_binarySearchValid :: UTCTime -> Series Int -> Bool
prop_binarySearchValid t xs  = linearSearch t xs == binarySearch t xs

binarySearchValidProperty :: Property
binarySearchValidProperty =
  property prop_binarySearchValid
