module Resample (props, unit) where

import Control.Arrow (first)
import Data.Series (emptySeries, resampleSAH, series)
import Data.Time (UTCTime (..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Series (Series)
import Test.QuickCheck (Property)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (property, testProperty)
import Utils (mkUTCTime)

ts0 :: Vector UTCTime
ts0 = Vector.fromList $ mkUTCTime <$> [1, 4, 6]

xs0 :: Series Char
xs0 = series $ map (first mkUTCTime) [(0, 'a'), (3, 'b'), (5, 'c'), (7, 'd')]

ys0 :: Series Char
ys0 = series $ map (first mkUTCTime) [(1, 'a'), (4, 'b'), (6, 'c')]

ts1 :: Vector UTCTime
ts1 = Vector.fromList $ mkUTCTime <$> [1, 4, 8]

ys1 :: Series Char
ys1 = series $ map (first mkUTCTime) [(1, 'a'), (4, 'b'), (8, 'd')]

ts2 :: Vector UTCTime
ts2 = Vector.fromList $ mkUTCTime <$> [0, 4, 6]

xs2 :: Series Char
xs2 = series $ map (first mkUTCTime) [(1, 'a'), (3, 'b'), (5, 'c'), (7, 'd')]

ys2 :: Series Char
ys2 = series $ map (first mkUTCTime) [(4, 'b'), (6, 'c')]

xs3 :: Series Char
xs3 = series $ map (first mkUTCTime) [(1, 'a'), (3, 'b'), (5, 'c')]

ts4 :: Vector UTCTime
ts4 = Vector.fromList $ mkUTCTime <$> [1, 3, 5]

-------------------------------------------------------------------------------

unit :: [TestTree]
unit =
  [ testCase "Resample unit 0" $ resampleSAH ts0 xs0 @?= ys0
  , testCase "Resample unit 1" $ resampleSAH ts1 xs0 @?= ys1
  , testCase "Resample unit 2" $ resampleSAH ts2 xs2 @?= ys2
  , testCase "Resample unit 3" $ resampleSAH ts2 xs3 @?= ys2
  , testCase "Resample unit 4" $ resampleSAH ts4 xs3 @?= xs3
  , testCase
      "Resample unit 5"
      $ resampleSAH Vector.empty emptySeries @?= emptySeries @Int
  ]

props :: [TestTree]
props =
  [ testProperty "resampleEmptyValidTs" resampleEmptyValidTsProperty
  , testProperty "resampleEmptyValidXs" resampleEmptyValidXsProperty
  ]

prop_resampleEmptyValidTs :: Series Int -> Bool
prop_resampleEmptyValidTs xs = resampleSAH Vector.empty xs == emptySeries

resampleEmptyValidTsProperty :: Property
resampleEmptyValidTsProperty = property prop_resampleEmptyValidTs

prop_resampleEmptyValidXs :: Vector UTCTime -> Bool
prop_resampleEmptyValidXs ts = resampleSAH ts emptySeries == emptySeries @Int

resampleEmptyValidXsProperty :: Property
resampleEmptyValidXsProperty = property prop_resampleEmptyValidXs
