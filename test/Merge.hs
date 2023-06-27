module Merge (props, unit) where

import Control.Arrow (first)
import Data.Series (bounds, emptySeries, merge, series)
import Data.Time (UTCTime)
import Series (Series)
import Test.QuickCheck (Property)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (property, testProperty)
import Utils (mkUTCTime)

mergeBounds :: Maybe (UTCTime, UTCTime) -> Maybe (UTCTime, UTCTime) -> Maybe (UTCTime, UTCTime)
mergeBounds (Just (al, au)) (Just (bl, bu)) = Just (min al bl, max au bu)
mergeBounds Nothing bs = bs
mergeBounds as Nothing = as

xs0 :: Series Char
xs0 = series $ map (first mkUTCTime) [(0, 'a'), (3, 'c'), (5, 'e'), (7, 'g')]

ys0 :: Series Char
ys0 = series $ map (first mkUTCTime) [(1, 'b'), (4, 'd'), (6, 'f')]

zs0 :: Series Char
zs0 = series $ map (first mkUTCTime) [(0, 'a'), (1, 'b'), (3, 'c'), (4, 'd'), (5, 'e'), (6, 'f'), (7, 'g')]

-------------------------------------------------------------------------------

unit :: [TestTree]
unit =
  [ testCase "Merge unit 0" $ merge xs0 ys0 @?= zs0
  ]

props :: [TestTree]
props =
  [ testProperty "merge empty with non-empty valid" mergeEmptyValidXs0Property
  , testProperty "merge non-empty with empty valid" mergeEmptyValidXs1Property
  , testProperty "mergeBounds (bounds a) (bounds b) == bounds (merge a b)" mergeBoundsValidProperty
  ]

prop_mergeEmptyValidXs0 :: Series Int -> Bool
prop_mergeEmptyValidXs0 xs = merge xs emptySeries == xs

mergeEmptyValidXs0Property :: Property
mergeEmptyValidXs0Property = property prop_mergeEmptyValidXs0

prop_mergeEmptyValidXs1 :: Series Int -> Bool
prop_mergeEmptyValidXs1 xs = merge xs emptySeries == xs

mergeEmptyValidXs1Property :: Property
mergeEmptyValidXs1Property = property prop_mergeEmptyValidXs1

prop_mergeBoundsValid :: Series Int -> Series Int -> Bool
prop_mergeBoundsValid as bs = mergeBounds (bounds as) (bounds bs) == (bounds $ merge as bs)

mergeBoundsValidProperty :: Property
mergeBoundsValidProperty = property prop_mergeBoundsValid
