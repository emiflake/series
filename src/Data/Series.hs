module Data.Series (
  DataPoint (..),
  Series,
  emptySeries,
  series,
  Data.Series.lookup,
  (!?),
  slice,
  size,
  singleton,
  merge,
  resampleSAH,
  bounds,
  isEmpty,
  pairWiseZipWith,
)
where

import Data.List (sortOn)
import Data.Series.Internal (DataPoint (..), Series (..), binarySearch, emptySeries, exact, inclusiveSlice, isEmpty, merge, resampleSAH, pairWiseZipWith)
import Data.Time (UTCTime)
import Data.Vector qualified as Vector
import Data.Series.TimeRange (TimeRange(TimeRange))

-- | Create a series with only a single data point.
singleton :: UTCTime -> a -> Series a
singleton t = Series . Vector.singleton . DataPoint t

size :: Series a -> Int
size (Series dps) = Vector.length dps

-- | Create a 'Series' from an association list. This will sort the list.
series :: [(UTCTime, a)] -> Series a
series = Series . Vector.fromList . sortOn (.time) . fmap (uncurry DataPoint)

-- | Slice a series from one time to another. Inclusive. /O(log n)/.
slice :: Show a => UTCTime -> UTCTime -> Series a -> Series a
slice _ _ s | isEmpty s = emptySeries
slice start end s@(Series dps) =
  case sl of
    Nothing -> emptySeries
    Just (a, b) -> Series $ Vector.slice a ((b + 1) - a) dps
  where
    sl = do a <- binarySearch start s; b <- binarySearch end s; inclusiveSlice a b

{- | Find the value at a specific time.

     /O(log n)/.
-}
lookup :: UTCTime -> Series a -> Maybe a
lookup t s = fmap ((.value) . snd) $ exact =<< binarySearch t s

{- | Infix version of 'Data.Series.lookup'.

     /O(log n)/.
-}
(!?) :: forall a. Series a -> UTCTime -> Maybe a
(!?) = flip Data.Series.lookup

{- | Compute the bounds of a 'Series'.

     /O(1)/.
-}
bounds :: forall a. Series a -> Maybe TimeRange
bounds (Series s) | Vector.null s = Nothing
bounds (Series s) = Just $ TimeRange (Vector.head s).time (Vector.last s).time
