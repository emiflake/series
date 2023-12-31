module PointwiseZipWith (unit, props) where

import Data.Series (Series, pointwiseZipWith, series)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (mkUTCTime)

xs :: Series Rational
xs =
  series
    [ (mkUTCTime 0, 1)
    , (mkUTCTime 4, 1.2)
    , (mkUTCTime 10, 1.4)
    , (mkUTCTime 200, 1.2)
    , (mkUTCTime 2500, 1.8)
    , (mkUTCTime 2700, 2)
    ]

ys :: Series Rational
ys =
  series
    [ (mkUTCTime 0, 100_000_000)
    , (mkUTCTime 2, 75_000_000)
    , (mkUTCTime 150, 125_000_000)
    , (mkUTCTime 1000, 100_000_000)
    ]

zs :: Series Rational
zs =
  series
    [ (mkUTCTime 0, 100_000_000)
    , (mkUTCTime 0, 100_000_000)
    , (mkUTCTime 2, 75_000_000)
    , (mkUTCTime 4, 90_000_000)
    , (mkUTCTime 10, 105_000_000)
    , (mkUTCTime 150, 175_000_000)
    , (mkUTCTime 200, 150_000_000)
    , (mkUTCTime 1000, 120_000_000)
    , (mkUTCTime 2500, 180_000_000)
    , (mkUTCTime 2700, 200_000_000)
    ]

unit :: [TestTree]
unit =
  [ testCase "Pointwise zip with unit 0" $ pointwiseZipWith (*) xs ys @?= zs
  ]

props :: [TestTree]
props = []
