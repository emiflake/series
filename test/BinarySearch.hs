module BinarySearch (props) where

import Data.Series.Internal (binarySearch, linearSearch)
import Data.Time (UTCTime (..))
import Series
import Test.QuickCheck (Property, property)
import Test.Tasty (TestTree, adjustOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize (..), QuickCheckTests (..), testProperty)

props :: [TestTree]
props =
  [ adjustOption (\_ -> QuickCheckTests 1000) $ adjustOption (\_ -> QuickCheckMaxSize 1000) $ testProperty "binarySearchValid" binarySearchValidProperty
  ]

prop_binarySearchValid :: UTCTime -> Series Int -> Bool
prop_binarySearchValid t xs = linearSearch t xs == binarySearch t xs

binarySearchValidProperty :: Property
binarySearchValidProperty =
  property prop_binarySearchValid
