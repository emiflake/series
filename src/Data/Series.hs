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
  resampleSH
)
where

import Data.List (sortOn)
import Data.Series.Internal (DataPoint (..), Series (..), binarySearch, exact, inclusiveSlice)
import Data.Time (UTCTime)
import Data.Vector qualified as Vector
import qualified Data.Vector.Mutable as MVector
import Data.STRef (newSTRef, readSTRef, modifySTRef)
import Data.Foldable (for_)
import Data.Vector (Vector)

findLargestSmallerThan
  :: forall a
   . UTCTime
  -> Series a
  -> Maybe (DataPoint a)
findLargestSmallerThan t (Series xs) =
  lastMaybe $ Vector.filter
    (\(DataPoint t0 _) -> t0 <= t)
    xs
  where
    lastMaybe :: forall b. Vector b -> Maybe b
    lastMaybe xs' | Vector.null xs' = Nothing
    lastMaybe xs' = Just $ Vector.last xs'

-- iterate over ts, find largest x.time so that x.time < t
-- set new[j] = x.time
resampleSH :: forall a. Vector UTCTime -> Series a -> Series a
resampleSH _ xs | isEmpty xs = emptySeries
resampleSH ts xs =
  Series $ Vector.create $ do
    let tLength = Vector.length ts
    nv <- MVector.new tLength
    a <- newSTRef @Int 0
    let writeAtFrom i ref v = do
          MVector.write nv i $ v
          modifySTRef ref succ
    for_ [0..tLength - 1] $ \i -> do
      ai <- readSTRef a
      case findLargestSmallerThan (ts Vector.! i) xs of
        Nothing -> pure ()
        Just t -> writeAtFrom ai a t
    ai <- readSTRef a
    pure $ MVector.slice 0 ai nv

-- | Series with no data points.
emptySeries :: Series a
emptySeries = Series Vector.empty

-- | Create a series with only a single data point.
singleton :: UTCTime -> a -> Series a
singleton t = Series . Vector.singleton . DataPoint t

size :: Series a -> Int
size (Series dps) = Vector.length dps

-- | Create a 'Series' from an association list. This will sort the list.
series :: [(UTCTime, a)] -> Series a
series = Series . Vector.fromList . sortOn (.time) . fmap (uncurry DataPoint)

-- | True if a 'Series' has any elements.
isEmpty :: Series a -> Bool
isEmpty (Series dps) = Vector.null dps

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

(!?) :: forall a. Series a -> UTCTime -> Maybe a
(!?) = flip Data.Series.lookup

-- | Merge two series, preserving temporal order.
-- |
-- | /O(n+m)/.
merge :: Series a -> Series a -> Series a
merge (Series dpsA) (Series dpsB) =
  Series $ Vector.create $ do
    let newLength = Vector.length dpsA + Vector.length dpsB
    nv <- MVector.new newLength
    a <- newSTRef @Int 0
    b <- newSTRef @Int 0
    let writeAtFrom i ref v = do
          MVector.write nv i $ v
          modifySTRef ref succ
          
    for_ [0..newLength - 1] $ \i -> do
      ai <- readSTRef a
      bi <- readSTRef b
      case (dpsA Vector.!? ai, dpsB Vector.!? bi) of
        (Just av, Nothing) -> writeAtFrom i a av
        (Nothing, Just bv) -> writeAtFrom i b bv
        (Just av, Just bv) ->
          case compare av.time bv.time of
            EQ -> writeAtFrom i a av
            LT -> writeAtFrom i a av
            GT -> writeAtFrom i b bv
        (Nothing, Nothing) ->
          -- FIXME(Emily, 26 June 2023): This should never happen, but let's be lenient for now.
          pure ()
    pure nv
