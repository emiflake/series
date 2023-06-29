module Main (main) where

import Criterion (bench, bgroup, env)
import Criterion.Main (defaultMain, whnf)
import Data.Series (Series, series)
import Data.Series qualified as Series
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime), secondsToDiffTime)
import Naive qualified

mkUTCTime :: Integral a => a -> UTCTime
mkUTCTime x =
  UTCTime
    (ModifiedJulianDay $ toInteger x `div` 86401)
    (secondsToDiffTime $ toInteger x `mod` 86401)

setupEnv :: IO (Series Int, Naive.Series Int)
setupEnv = do
  let largeSeries :: Series Int
      largeSeries = series [(mkUTCTime x, x) | x <- [0 .. 1000000]]

      naiveLargeSeries :: Naive.Series Int
      naiveLargeSeries = Naive.series [(mkUTCTime x, x) | x <- [0 .. 1000000]]
  pure (largeSeries, naiveLargeSeries)

main :: IO ()
main =
  defaultMain
    [ env setupEnv $ \ ~(largeSeries, naiveLargeSeries) ->
        bgroup
          "slice"
          [ bench "series" $ whnf (Series.slice (mkUTCTime @Int 50000) (mkUTCTime @Int 100000)) largeSeries
          , bench "naive" $ whnf (Naive.slice (mkUTCTime @Int 50000) (mkUTCTime @Int 100000)) naiveLargeSeries
          ]
    ]
