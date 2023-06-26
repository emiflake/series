module Main (main) where

import Prelude

import BinarySearch qualified
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import Resample qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "test suite"
      [ testGroup "Binary search properties" BinarySearch.props
      , testGroup "Resample properties" Resample.props
      , testGroup "Resample unit tests" Resample.unit
      ]
