module Nub (props) where

import Data.Function (on, (&))
import Data.List qualified as List
import Data.Ord (comparing)
import Data.Series (DataPoint (DataPoint))
import Data.Series qualified as Series
import Data.Vector qualified as Vector
import Series (Series (Series))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

props :: [TestTree]
props =
  [ testProperty "size (nub xs) <= size xs" (prop_lengthNubLess @Int)
  , testProperty "nub makes times unique" (prop_nubMakesUnique @Int)
  , testProperty "nubWith on head is nub" (prop_nubWithHeadIsNub @Int)
  , testProperty
      "nubWith compares to declarative implementation"
      (prop_nubWithDeclarative @Int)
  ]

prop_lengthNubLess :: forall a. Series a -> Bool
prop_lengthNubLess xs = Series.size (Series.nub xs) <= Series.size xs

prop_nubMakesUnique :: forall a. Series a -> Bool
prop_nubMakesUnique xs =
  Vector.toList (Series.times (Series.nub xs))
    == List.nub (Vector.toList (Series.times xs))

prop_nubWithHeadIsNub :: forall a. Eq a => Series a -> Bool
prop_nubWithHeadIsNub xs = Series.nubWith Vector.head xs == Series.nub xs

prop_nubWithDeclarative :: forall a. Ord a => Series a -> Bool
prop_nubWithDeclarative xs@(Series dps) =
  declarative == Series.nubWith Vector.maximum xs
  where
    declarative :: Series a
    declarative =
      Vector.toList dps
        & List.groupBy ((==) `on` (.time))
        & fmap
          ( (\(DataPoint t v) -> (t, v))
              . List.maximumBy (comparing (.value))
          )
        & Series.series
