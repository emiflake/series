module Merge (props, unit) where

import Data.Series (bounds, emptySeries, merge, series)
import Series (Series)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Utils (mkUTCTime)

xs0 :: Series Char
xs0 = series [(mkUTCTime i, v) | (i, v) <- [(0, 'a'), (3, 'c'), (5, 'e'), (7, 'g')]]
ys0 :: Series Char
ys0 = series [(mkUTCTime i, v) | (i, v) <- [(1, 'b'), (4, 'd'), (6, 'f')]]

zs0 :: Series Char
zs0 = series [(mkUTCTime i, v) | (i, v) <- [(0, 'a'), (1, 'b'), (3, 'c'), (4, 'd'), (5, 'e'), (6, 'f'), (7, 'g')]]

-------------------------------------------------------------------------------

unit :: [TestTree]
unit =
  [ testCase "Merge unit 0" $ merge xs0 ys0 @?= zs0
  ]

props :: [TestTree]
props =
  [ testProperty "Merge with empty on left is identity" prop_mergeEmptyLeftIdentity
  , testProperty "Merge with empty on right is identity" prop_mergeEmptyRightIdentity
  , testProperty "bounds a <> bounds b == bounds (merge a b)" prop_mergeBoundsDistributive
  ]

prop_mergeEmptyLeftIdentity :: Series Int -> Bool
prop_mergeEmptyLeftIdentity xs = merge emptySeries xs == xs

prop_mergeEmptyRightIdentity :: Series Int -> Bool
prop_mergeEmptyRightIdentity xs = merge xs emptySeries == xs

prop_mergeBoundsDistributive :: Series Int -> Series Int -> Bool
prop_mergeBoundsDistributive as bs = bounds as <> bounds bs == bounds (merge as bs)
