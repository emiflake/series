module Main (main) where

import Prelude

import BinarySearch qualified
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import Resample qualified
import Test.Tasty (defaultMain, testGroup, adjustOption)
import Test.Tasty.QuickCheck (QuickCheckTests(QuickCheckTests), QuickCheckMaxSize (QuickCheckMaxSize))

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
      ]
