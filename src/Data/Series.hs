module Data.Series
  ( DataPoint(..)
  , Series
  , emptySeries
  , series
  , Data.Series.lookup
  , (!?)
  , slice
  )
  where

import Data.Series.Internal (Series(..), DataPoint(..), binarySearch, exact, latest, inclusiveSlice)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.These (These(These, This, That), these)
import qualified Debug.Trace as Debug
import Data.Maybe (fromJust)
import Data.List (sortOn)

-- | Series with no data points.
emptySeries :: Series a
emptySeries = Series Vector.empty

-- | Create a series with only a single data point.
singleton :: UTCTime -> a -> Series a
singleton t = Series . Vector.singleton . DataPoint t

-- | Create a 'Series' from an association list. This will sort the list.
series :: [(UTCTime, a)] -> Series a
series = Series . Vector.fromList . sortOn (.time) . fmap (uncurry DataPoint)

-- | True if a 'Series' has any elements.
isEmpty :: Series a -> Bool
isEmpty (Series dps) = Vector.null dps

-- | Slice a series from one time to another. Inclusive. /O(log n)/.
slice :: Show a => UTCTime -> UTCTime -> Series a -> Series a
slice _ _ series | isEmpty series = emptySeries
slice start end series@(Series dps) =
  case sl of
    Nothing -> emptySeries
    Just (a, b) -> Series $ Vector.slice a ((b + 1) - a) dps
  where
    sl = do { a <- binarySearch start series; b <- binarySearch end series; inclusiveSlice a b }

-- | Find the value at a specific time.
--
-- /O(log n)/.
lookup :: UTCTime -> Series a -> Maybe a
lookup t s = fmap ((.value). snd) $ exact =<< binarySearch t s 

(!?) :: forall a. Series a -> UTCTime -> Maybe a
(!?) = flip Data.Series.lookup

  
