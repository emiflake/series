module Data.Series (
  DataPoint (..),
  Series (getSeries),
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
  pointwiseZipWith,
  nub,
  nubWith,
  values,
  times,
)
where

import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef)
import Data.STRef.Strict (modifySTRef)
import Data.Series.Internal (DataPoint (..), Series (..), binarySearch, emptySeries, exact, inclusiveSlice, isEmpty)
import Data.Series.TimeRange (TimeRange (TimeRange))
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector

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

findLargestSmallerThan ::
  forall a.
  UTCTime ->
  Series a ->
  Maybe (DataPoint a)
findLargestSmallerThan t (Series xs) =
  lastMaybe $
    Vector.filter
      (\(DataPoint t0 _) -> t0 <= t)
      xs
  where
    lastMaybe :: forall b. Vector b -> Maybe b
    lastMaybe xs' | Vector.null xs' = Nothing
    lastMaybe xs' = Just $ Vector.last xs'

{- | Merge two series, preserving temporal order.

     /O(n+m)/.
-}
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

    for_ [0 .. newLength - 1] $ \i -> do
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

{- | Create a new series with the times in the given vector, at each of the
     times in the vector place the last registered value in the given series
     before that time.

     If a given time from the vector is before the first time in the series,
     drop it.

     Passing an empty vector or series will always result in an empty series.
-}
resampleSAH :: forall a. Vector UTCTime -> Series a -> Series a
resampleSAH _ xs | isEmpty xs = emptySeries
resampleSAH ts xs =
  Series $ Vector.create $ do
    let tLength = Vector.length ts
    nv <- MVector.new tLength
    a <- newSTRef @Int 0
    let writeAtFrom i ref v = do
          MVector.write nv i $ v
          modifySTRef ref succ
    for_ [0 .. tLength - 1] $ \i -> do
      ai <- readSTRef a
      let t = ts Vector.! i
      case findLargestSmallerThan t xs of
        Nothing -> pure ()
        Just (DataPoint _ x) -> writeAtFrom ai a $ DataPoint t x
    ai <- readSTRef a
    pure $ MVector.slice 0 ai nv

sortedVectorNub :: forall a. Ord a => Vector (DataPoint a) -> Vector (DataPoint a)
sortedVectorNub v =
  Vector.ifilter
    (\i dp -> not $ smallerThanPrevious i dp || smallerThanNext i dp)
    v
  where
    len :: Int
    len = length v

    previous :: Int -> DataPoint a
    previous i = v Vector.! (i - 1)

    next :: Int -> DataPoint a
    next i = v Vector.! (i + 1)

    smallerThanNext :: Ord a => Int -> DataPoint a -> Bool
    smallerThanNext i dp =
      i < len - 1
        && dp.time == (next i).time
        && dp.value <= (next i).value

    smallerThanPrevious :: Ord a => Int -> DataPoint a -> Bool
    smallerThanPrevious i dp =
      i > 0
        && dp.time == (previous i).time
        && dp.value < (previous i).value

-- | Merge two series, applying the latest value from each with the other.
pointwiseZipWith :: forall a b c. Ord c => (a -> b -> c) -> Series a -> Series b -> Series c
pointwiseZipWith f sa sb = Series $ sortedVectorNub $ fmap (\dp -> dp {value = f (fromJust $ Data.Series.lookup dp.time resampledSeriesA) dp.value}) $ deconstr resampledSeriesB
  where
    resampledSeriesA :: Series a
    resampledSeriesA = applySAH (times sb) sa

    resampledSeriesB :: Series b
    resampledSeriesB = applySAH (times sa) sb

    deconstr :: forall d. Series d -> Vector (DataPoint d)
    deconstr (Series s) = s

-- | Extract the times vector from the series.
times :: forall a. Series a -> Vector UTCTime
times (Series s) = Vector.map (.time) s

-- | Extract only the values from the 'Series'.
values :: Series a -> Vector a
values (Series s) = Vector.map (.value) s

-- | Add additional data points based on existing data in the 'Series'.
applySAH :: forall a. Vector UTCTime -> Series a -> Series a
applySAH ts s = merge s $ resampleSAH ts s

{- | Remove duplicates without care for preserving some order of elements.

     /O(n)/.
-}
nub :: Series a -> Series a
nub (Series dps) = Series $ Vector.ifilter (\i dp -> maybe True ((dp.time /=) . (.time)) $ dps Vector.!? (i - 1)) dps

-- | Remove duplicates from a series, exposing the choice for preserving element to the caller.
nubWith :: forall a. (Vector a -> a) -> Series a -> Series a
nubWith f (Series dps) =
  Series $ Vector.create $ do
    let len = Vector.length dps
    count <- newSTRef @Int 0
    nv <- MVector.new len
    let loop s | Vector.null s = MVector.slice 0 <$> (readSTRef count) <*> pure nv
        loop s = do
          let candidate = Vector.head s
              (cs, rest) = Vector.span (\e -> e.time == candidate.time) s
          count' <- readSTRef count
          MVector.write nv count' (DataPoint candidate.time . f . Vector.map (.value) $ cs)
          modifySTRef count succ
          loop rest
    loop dps
