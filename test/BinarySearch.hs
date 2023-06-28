module BinarySearch (props) where

import Data.Kind (Type)
import Data.Series (DataPoint (..))
import Data.Series.Internal (SearchResult (ExactMatch, Nearest), binarySearch)
import Data.These (These (That, These, This))
import Data.Time (UTCTime (..))
import Data.Vector qualified as Vector
import Series (Series (Series))
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

linearSearch ::
  forall (a :: Type).
  UTCTime ->
  Series a ->
  Maybe (SearchResult a)
linearSearch _t (Series xs) | Vector.null xs = Nothing
linearSearch t (Series xs) =
  Just $ go (Vector.head xs) 0
  where
    len = Vector.length xs

    go :: DataPoint a -> Int -> SearchResult a
    go dp i | dp.time == t = ExactMatch i dp
    go dp 0 | dp.time > t = Nearest (That (0, dp))
    go dp i
      | dp.time > t =
          Nearest (These (i - 1, xs Vector.! (i - 1)) (i, dp))
    go dp i | i == len - 1 = Nearest (This (i, dp))
    go _ i = go (xs Vector.! (i + 1)) (i + 1)
