module BinarySearch (props) where

import Data.Series.Internal (binarySearch, linearSearch)
import Data.Time (UTCTime (..))
import Series
import Test.QuickCheck (Property, property)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

props :: [TestTree]
props =
  [ testProperty "binarySearchValid" binarySearchValidProperty
  ]

prop_binarySearchValid :: UTCTime -> Series Int -> Bool
prop_binarySearchValid t xs = linearSearch t xs == binarySearch t xs

binarySearchValidProperty :: Property
binarySearchValidProperty =
  property prop_binarySearchValid
