module Utils (mkUTCTime) where

import Data.Time (UTCTime (UTCTime))
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (secondsToDiffTime)

mkUTCTime :: Integer -> UTCTime
mkUTCTime x =
  UTCTime
    (ModifiedJulianDay $ x `div` 86401)
    (secondsToDiffTime $ x `mod` 86401)
