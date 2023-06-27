module Merge (props, unit) where

import Data.Series (emptySeries, resampleSH, series)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (secondsToDiffTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Test.Tasty (TestTree, adjustOption)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (Property)
import Test.Tasty.QuickCheck (property, testProperty, QuickCheckMaxSize (QuickCheckMaxSize), QuickCheckTests (QuickCheckTests))
import Series (Series)

mkUTCTime :: Integer -> UTCTime
mkUTCTime x =
  UTCTime
    (ModifiedJulianDay $ x `div` 86401)
    (secondsToDiffTime $ x `mod` 86401)

mapFst :: Functor f => (a -> c) -> f (a, b) -> f (c, b)
mapFst f = (<$>) (\(x, y) -> (f x, y))

ts0 :: Vector UTCTime
ts0 = Vector.fromList $ mkUTCTime <$> [1, 4, 6]

xs0 :: Series Char
xs0 = series $ mapFst mkUTCTime [(0, 'a'), (3, 'b'), (5, 'c'), (7, 'd')]

ys0 :: Series Char
ys0 = series $ mapFst mkUTCTime [(0, 'a'), (3, 'b'), (5, 'c'), (7, 'd')]

-------------------------------------------------------------------------------

unit :: [TestTree]
unit =
  [ testCase "Merge unit 0" $ resampleSH ts0 xs0 @?= ys0
  ]

props :: [TestTree]
props =
  [ adjustOption (\_ -> QuickCheckTests 1000) $ adjustOption (\_ -> QuickCheckMaxSize 1000) $ testProperty "resampleEmptyValidTs" resampleEmptyValidTsProperty
  , adjustOption (\_ -> QuickCheckTests 1000) $ adjustOption (\_ -> QuickCheckMaxSize 1000) $ testProperty "resampleEmptyValidXs" resampleEmptyValidXsProperty
  ]

prop_resampleEmptyValidTs :: Series Int -> Bool
prop_resampleEmptyValidTs xs = resampleSH Vector.empty xs == emptySeries

resampleEmptyValidTsProperty :: Property
resampleEmptyValidTsProperty = property prop_resampleEmptyValidTs

prop_resampleEmptyValidXs :: Vector UTCTime -> Bool
prop_resampleEmptyValidXs ts = resampleSH ts emptySeries == emptySeries @Int

resampleEmptyValidXsProperty :: Property
resampleEmptyValidXsProperty = property prop_resampleEmptyValidXs
