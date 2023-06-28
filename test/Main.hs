module Main (main) where

import Prelude

import BinarySearch qualified
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import Merge qualified
import PointwiseZipWith qualified
import Resample qualified
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckMaxSize (QuickCheckMaxSize), QuickCheckTests (QuickCheckTests))
import qualified Nub

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    adjustOption (\_ -> QuickCheckTests 1000) $
      adjustOption (\_ -> QuickCheckMaxSize 1000) $
        testGroup
          "test suite"
          [ testGroup "Binary search properties" BinarySearch.props
          , testGroup "Resample properties" Resample.props
          , testGroup "Resample unit tests" Resample.unit
          , testGroup "Merge properties" Merge.props
          , testGroup "Merge unit tests" Merge.unit
          , testGroup "Pointwise zip with properties tests" PointwiseZipWith.props
          , testGroup "Pointwise zip with unit tests" PointwiseZipWith.unit
          , testGroup "Nub property tests" Nub.props
          ]
