module Main (main) where

import Prelude

import qualified BinarySearch
import GHC.IO.Encoding (setLocaleEncoding)
import Test.Tasty (defaultMain, testGroup)
import GHC.IO.Encoding.UTF8 (utf8)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "test suite"
      [ testGroup "Binary search properties" BinarySearch.props
      ]
